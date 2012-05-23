package ppl.dsl.optila.matrix

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila._

/**
 * This file implements the Matrix interface for a CSR SparseMatrix.
 * 
 * Many (all?) of the operations in the interface are not efficient with this representation and should use another representation
 * instead. Should that conversion be automatic?
 *   
 * For generic Delite ops, we would likely prefer something like a COO representation. For fast arithmetic, CSR is good - but the arithmetic
 * ops must be specialized inside the SparseMatrixOps (cannot rely on the generic impls in MatrixOps).
 * 
 * Option a)
 *   use SparseMatrixCOO to build the SparseMatrix result of a Delite op (only implement mutable operations for SparseMatrixCOO)
 *   automatically convert the SparseMatrixCOO to a SparseMatrixCSR at the end of the op?
 * 
 * Option b) 
 *   do not bother with Delite ops directly on sparse matrices. Do not support the Interface[Matrix].
 *   explicitly construct sparse matrices as SparseMatrixCOO, then convert to an immutable SparseMatrixCSR
 *   only support specialized, arithmetic / bulk operators that go from SparseMatrixCSR -> SparseMatrixCSR
 * 
 * If we adopt Option (b), should sparse vectors have the same model? (then there was no point to changing delite ops...)
 *
 * What are the characteristics of the solution we would like ideally?
 *  1) high performance: it should be hard to write inefficient code
 *  2) unified model: SparseMatrix isn't a special case in the implementation
 *  3) simple: the user should not have to deal with any more representation details than absolutely necessary
 * 
 * val m = SparseMatrix.construct(...)   // implicitly COO during construction, returns immutable CSR
 * val t1 = m map { foo }                // compute the map on 0 once, write that value to all non-zeros; run the map on nnz for remaining
 *                                          build up the result as a COO, convert to immutable CSR on return
 * val t2 = m mapNZ { foo }              // operates on nz values
 * val t3 = m + m                        // operates on nz values
 * 
 * The SparseMatrix builder should allocate a COO. dc_update and dc_append should treat it as a COO. dc_apply should treat it as a CSR?
 * 
 * Option c) transformation  (TODO: test with sparse vectors BEFORE proceeding with sparse matrix impl)
 * 
 * Is it possible to go from
 *  val t1 = m map { e => foo(e) }
 * 
 * to:
 *  val x1 = foo(0)
 *  val x2 = m.mapNZWithDefault(e => foo(e), foo(0)) // can do different things if foo(0) == 0 or not
 * 
 * not by construction, but strictly by analyzing/optimizing the original map operation? in other words, can we handle this at the delite op level?
 * 
 * Using ForwardTransformers:
 * 
 * let's say we look at a DeliteOpMap(intf, f), where intf: Interface[Matrix[A]]. we can dispatch on intf.mM[A] to see if it's a manifest for a SparseMatrix[A].
 *  if it is, we can transform it as above!
 * 
 * same thing with Zip:
 * (do different things if just one of the inputs is sparse)
 * DeliteOpZipWith(intfA, intfB, f) if intfA == SparseVector && intfB == SparseVector => 
 *  test the zip function - this is essentially recovering the operator info!: (how would we get nonzero values to test with? nonDefaultValue[A]? sound?)
 *    zip(a,0) && zip(0,b) = 0 => apply zip function only on intersection(nz(a),nz(b))
 *    zip(a,0) = 0 => apply zip function only on nz(b)
 *    zip(0,b) = 0 => apply zip function only on nz(a)
 *    zip(0,0) = 0 => apply zip function only on union(nz(a),nz(b))
 *  if testing the zip function is infeasible, can we pass the operator info along with the zip in a context parameter? e.g. + is a zip(bothArgsMustBeZero) kind of thing
 *      
 * SparseMatrix should:
 *   1) use the transformation trick to lower generic maps and zips to their more efficient version if they're sparse [thus, reusing interface!]
 *   2) add a finalize method to MatrixBuilder: 
 *      - alloc constructs the output as a COO 
 *      - finalize transforms it and returns it as an immutable CSR (as discussed above)
 *      - zips only occur on CSRs!
 *      use the type system to force finalize to be called. have alloc return a Rep[NeedToBeFinalized], and finalize return a Rep[MB]
 * 
 * if we do the transformations right, there should be no fallback case! (so the collectelem interface really didn't need to change after all!)
 * thus, all mutable ops in SparseMatrix can just throw an exception; we can explicitly construct COO in our sparse constructors, and everything would stay CSR after that
 *    - this is actually nice because it guarantees the high performance transformation happened. any case where it couldn't?
 *    - theoretically dc ops for sparsevector/sparsematrix could all throw "should have been transformed" runtime exceptions
 *    ??? there is a lot of stuff in vector/matrix impl that relies on mutability, particular alloc -> update to implement sequential ops. that stuff would all be broken.
 *      - this stuff would be "ok", not great, if it was allocated as COO and "somehow" auto-converted to CSR upon return. override unsafeImmutable to convert return?
 *      - the actual operations would still be quite slow unless they were overridden directly inside sparse vector/matrix, because we can't auto-specialize them
 * 
 * Note that this may interfere with fusion, since in the lowering process the optimized op will have write effects to set data fields. But surely we can fix fusion to handle this.
 * 
 */
trait SparseMatrixCSRImplOps extends SparseMatrixImplOps {
  this: OptiLACompiler with SparseMatrixCSRCompilerOps with OptiLALift =>
  

  ///////////////
  // kernels
  
  def sparsematrix_vview_impl[A:Manifest](x: Rep[SparseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[VectorView[A]] = {
    throw new UnsupportedOperationException("tbd")
    //VectorView[A](sparsematrix_raw_data(x), start, stride, length, isRow)
  }
  
  protected def sparsematrix_csr_find_offset[A:Manifest](m: Rep[SparseMatrix[A]], row: Rep[Int], col: Rep[Int]): Rep[Int] = {
    val rowPtr = sparsematrix_csr_raw_rowptr(m)
    val colIndices = sparsematrix_csr_raw_colindices(m)
    // colIndices will be sorted between [rowPtr(row), rowPtr(row+1))
    bsearch(colIndices, rowPtr(row), rowPtr(row+1)-1, col)
  }
  
  def sparsematrix_apply_impl[A:Manifest](m: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A] = {    
    val data = sparsematrix_csr_raw_data(m)
    val offRaw = sparsematrix_csr_find_offset(m, i, j)
    if (offRaw > -1) data(offRaw) else defaultValue[A]    
  }
  
  def sparsematrix_update_impl[A:Manifest](m: Rep[SparseMatrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit] = {
    warn("possible performance problem - writing to a sparse matrix CSR representation")    
    
    val offRaw = sparsematrix_csr_find_offset(m,i,j)    
    if (offRaw > -1) darray_unsafe_update(sparsematrix_csr_raw_data(m), offRaw, y)
    else {
      // TODO AKS: test this
      if (y != defaultValue[A]) {
        val off = ~offRaw
        sparsematrix_csr_insertspace(m, off, 1)
        darray_unsafe_update(sparsematrix_csr_raw_colindices(m), off, j)
        darray_unsafe_update(sparsematrix_csr_raw_data(m), off, y)     
        val rowPtr = sparsematrix_csr_raw_rowptr(m)
        // have to increment every element of rowPtr
        for (row <- i until rowPtr.length) {
          darray_unsafe_update(rowPtr,row,rowPtr(row)+1)
        }
      }
    }        
  }

  protected def sparsematrix_csr_ensureextra[A:Manifest](m: Rep[SparseMatrix[A]], extra: Rep[Int]): Rep[Unit] = {
    if (sparsematrix_csr_raw_data(m).length - m.nnz < extra) {
      sparsematrix_csr_realloc(m,m.nnz + extra)
    }
  }
  
  protected def sparsematrix_csr_realloc[A:Manifest](m: Rep[SparseMatrix[A]], minLen: Rep[Int]): Rep[Unit] = {
    val data = sparsematrix_csr_raw_data(m)
    val colIndices = sparsematrix_csr_raw_colindices(m)
    var n = max(4, data.length * 2)
    while (n < minLen) n = n*2
    val dataOut = DeliteArray[A](n)
    val colIndicesOut = DeliteArray[Int](n)
    darray_unsafe_copy(data, 0, dataOut, 0, m.nnz)
    darray_unsafe_copy(colIndices, 0, colIndicesOut, 0, m.nnz)
    sparsematrix_csr_set_raw_data(m, dataOut.unsafeImmutable)
    sparsematrix_csr_set_raw_colindices(m, colIndicesOut.unsafeImmutable)
  }
  
  protected def sparsematrix_csr_insertspace[A:Manifest](m: Rep[SparseMatrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    sparsematrix_csr_ensureextra(m,len)
    val data = sparsematrix_csr_raw_data(m)
    val colIndices = sparsematrix_csr_raw_colindices(m)
    darray_unsafe_copy(data, pos, data, pos + len, m.nnz - pos)
    darray_unsafe_copy(colIndices, pos, colIndices, pos + len, m.nnz - pos)
    sparsematrix_set_nnz(m, m.nnz + len)
  }

  /*
  def sparsematrix_multiply_impl[A:Manifest:Arith](x: Rep[SparseMatrix[A]], y: Rep[SparseMatrix[A]]): Rep[SparseMatrix[A]] = {

    val yTrans = y.t
    val out = SparseMatrix[A](x.numRows, y.numCols)

    for (rowIdx <- 0 until x.numRows) {
      var i = unit(0)
      while (i < out.numCols) {
        var j = unit(1)
        var acc = x(rowIdx, 0) * yTrans(i, 0)
        while (j < yTrans.numCols) {
          acc += x(rowIdx, j) * yTrans(i, j)
          j += 1
        }
        out(rowIdx, i) = acc
        i += 1
      }
    }
    out.unsafeImmutable
  }
  
  // TODO: try/catch, case, in embedded implementation? we need to lift these still.
  def sparsematrix_inverse_impl[A:Manifest](m: Rep[SparseMatrix[A]])(implicit conv: Rep[A] => Rep[Double]): Rep[SparseMatrix[Double]] = {
    //m.chkEquals(m.numCols, m.numRows)

    // augment the SparseMatrix with the identity SparseMatrix of the same size
    val id = SparseMatrix.identity(m.numCols)
    val augMat = m.toDouble.mutable
    augMat.insertAllCols(augMat.numCols, id)
//    try{
      // perform row reductions
      val redMat = sparsematrix_rreduce(augMat)
      // strip off the augmented SparseMatrix
      redMat.removeCols(0, m.numCols)
      redMat.unsafeImmutable
//    }
//    catch{
//      case e: Exception => {
//        println("Error: SparseMatrix appears to be singular")
//        throw new IllegalArgumentException
//      }
//    }
  }

   protected def sparsematrix_rreduce(m: Rep[SparseMatrix[Double]]): Rep[SparseMatrix[Double]] = {
    // assumes m is mutable
    val currentMat = m
    var lead = unit(0)
    var finished = unit(false)
    var r = unit(0)

    while (!finished && r < m.numRows) {
      if (m.numRows <= lead){
        finished = true
      }
      if (!finished){
        var i = r
        while (!finished && currentMat(i, lead) == 0.0){
          i += 1
          if (m.numCols == i){
            i = r
            lead += 1
            if (m.numRows == lead) {
              finished = true
            }
          }
        }

        if (!finished){
          val tmpRow = currentMat(i).Clone
          currentMat(i) = currentMat(r)
          currentMat(r) = tmpRow
          currentMat(r) = currentMat(r) / currentMat(r,lead)

          for (i <- 0 until m.numRows){
            if (i != r)
              currentMat(i) = currentMat(i) - currentMat(r)*currentMat(i,lead)
          }
          lead += 1
        }
      }
      r += 1
    }

    currentMat
  }
  
  def sparsematrix_reducerows_impl[A:Manifest](x: Rep[SparseMatrix[A]], func: (Rep[SparseVector[A]], Rep[VectorView[A]]) => Rep[SparseVector[A]]): Rep[SparseVector[A]] = {
    var acc = ZeroVector[A](x.numCols)
    for (i <- 0 until x.numRows) {
      acc = func(acc, x(i))
    }
    acc
  }
  */
}