package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._
import scala.reflect.{SourceContext,RefinedManifest}

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.analysis._
import ppl.delite.framework.Util._

trait MultiArrayImpl[T] extends DeliteCollection[T]

trait MultiArrayPlainImpl[T] extends MultiArrayImpl[T]
trait MultiArrayViewImpl[T] extends MultiArrayImpl[T]
trait MultiArrayBuffImpl[T] extends MultiArrayImpl[T]
trait MultiArrayBuffViewImpl[T] extends MultiArrayImpl[T]

trait MultiArrayImplExp extends MultiArrayWrapExp with DeliteInternalOpsExp with DeliteSimpleOpsExp 
  with DeliteArrayBufferOpsExp with DeliteMapOpsExp with LayoutMetadataOps { self: DeliteOpsExp with DeliteFileReaderOpsExp => 

  val implementer: MultiArrayImplementer  

  def reductionTree(x: Seq[Exp[Int]])(f: (Exp[Int],Exp[Int]) => Exp[Int]): Seq[Exp[Int]] = {
    if (x.length == 1)
      x
    else if (x.length % 2 == 0)
      reductionTree(Seq.tabulate(x.length / 2){i => f(x(2*i), x(2*i + 1)) })(f)
    else
      reductionTree(Seq.tabulate(x.length / 2){i => f(x(2*i), x(2*i + 1)) } :+ x.last)(f)
  }
  def sumTree(x: Seq[Exp[Int]]): Exp[Int] = reductionTree(x){(a,b) => delite_int_plus(a,b)}.apply(0)
  def productTree(x: Seq[Exp[Int]]): Exp[Int] = reductionTree(x){(a,b) => delite_int_times(a,b)}.apply(0)

  def calcIndices(v: Exp[Int], dims: Seq[Exp[Int]]): Seq[Exp[Int]] = {
    if (dims.length == 1) { Seq(v) }
    else {
      Seq.tabulate(dims.length) { d => 
        if (d == dims.length - 1) { delite_int_mod(v, dims(d)) }
        else { delite_int_mod( delite_int_divide(v, productTree(dims.drop(d + 1))), dims(d)) }
      }
    }
  }

  // --- Convenience infix operations for DeliteMultiArray
  implicit def multiarrayToCastOps[T:Manifest](x: Exp[DeliteMultiArray[T]]) = new MultiArrayCastOpsCls[T](x)
  class MultiArrayCastOpsCls[T:Manifest](x: Exp[DeliteMultiArray[T]]) { 
    def asMultiArrayImpl = {
      assert(isMultiArrayImpl(x), "Cannot cast " + x.tp + " to MultiArrayImpl\n " + strDef(x))
      x.asInstanceOf[Exp[MultiArrayImpl[T]]]
    }
    def asMultiArrayViewImpl = {
      assert(isMultiArrayViewImpl(x), "Cannot cast " + x.tp + " to MultiArrayViewImpl\n" + strDef(x))
      x.asInstanceOf[Exp[MultiArrayViewImpl[T]]]
    }
    def asMultiArrayBuffImpl = {
      assert(isMultiArrayBuffImpl(x), "Cannot cast " + x.tp + " to MultiArrayBuffImpl\n" + strDef(x))
      x.asInstanceOf[Exp[MultiArrayBuffImpl[T]]]
    }
    def asMultiArrayBuffViewImpl = {
      assert(isMultiArrayBuffViewImpl(x), "Cannot cast " + x.tp + " to MultiArrayBuffViewImpl\n" + strDef(x))
      x.asInstanceOf[Exp[MultiArrayBuffViewImpl[T]]]
    }
  }

  implicit def multimapToCastOps[K:Manifest,V:Manifest](x: Exp[DeliteMultiMap[K,V]]) = new MultiMapCastOpsCls[K,V](x)
  class MultiMapCastOpsCls[K:Manifest,V:Manifest](x: Exp[DeliteMultiMap[K,V]]) {
    def asDeliteMap = {
      assert(isDeliteMap(x), "Cannot cast " + x.tp + " to DeliteMap")
      x.asInstanceOf[Exp[DeliteMap[K,V]]]
    }
  }

  // ---- Convenience infix operations for MultiArrayImpl and descendants
  implicit def multiarrayimplToOpsCls[T:Manifest](ma: Exp[MultiArrayImpl[T]]) = new MultiArrayImplOpsCls(ma)
  class MultiArrayImplOpsCls[T:Manifest](x: Exp[MultiArrayImpl[T]]) {
    def dims: Seq[Exp[Int]] = implementer.getDims(x.asInstanceOf[Exp[DeliteMultiArray[T]]])
    def dim(d: Int): Exp[Int] = implementer.implementDim(x.asInstanceOf[Exp[DeliteMultiArray[T]]], d)
    def size: Exp[Int] = implementer.implementSize(x.asInstanceOf[Exp[DeliteMultiArray[T]]])
    def strides: Seq[Exp[Int]] = implementer.getStrides(x.asInstanceOf[Exp[DeliteMultiArray[T]]])
    def stride(d: Int) = implementer.implementStride(x.asInstanceOf[Exp[DeliteMultiArray[T]]], d)
  }

  // --- Convenience infix operations for AbstractIndices
  implicit def indicesImplOps(x: Exp[AbstractIndices]) = new AbstractIndicesImplOpsCls(x)
  class AbstractIndicesImplOpsCls(x: Exp[AbstractIndices]) {
    def toSeq(n: Int): Seq[Exp[Int]] = Seq.tabulate(n){d => x(d)}
    def size(n: Int): Exp[Int] = productTree(toSeq(n))
  }

  object DMAStructView {
    def unapply[T](ma: Exp[DeliteMultiArray[T]]) = multiViewUnapply(ma)
  }

  def multiViewUnapply[T](ma: Exp[DeliteMultiArray[T]]): Option[(Seq[Exp[Int]], Seq[Exp[Int]])] = ma match {
    case _ => None
  }

  object DMAStruct {
    def unapply[T](ma: Exp[DeliteMultiArray[T]]) = multiUnapply(ma)
  }

  def multiUnapply[T](ma: Exp[DeliteMultiArray[T]]): Option[Seq[Exp[Int]]] = ma match {
    case _ => None
  }

  // --- MultiArray Implementation nodes

  case class ArrayMkString[A:Manifest](arr: Exp[DeliteMultiArray[A]], del: Exp[String], len: () => Exp[Int], stringify: Exp[Int] => Exp[String])(implicit ctx: SourceContext) extends DeliteOpSingleWithManifest[A,String](reifyEffectsHere{
    val size = len()
    if (delite_less_than(size, unit(1))) unit("[ ]")
    else {
      val i = var_new(unit(0))
      val s = var_new(unit(""))
      while(delite_less_than(i,delite_int_minus(size,unit(1)))) {
        s = delite_string_concat(readVar(s), stringify(readVar(i)) )
        s = delite_string_concat(readVar(s), del)
        i = delite_int_plus(readVar(i), unit(1))
      }
      delite_string_concat(readVar(s), stringify(readVar(i)))
    }
  }) { override def toString = "ArrayMkString(" + arr + ", " + del + ")" }

  // TODO: Use slices with array_mkstring calls instead?
  case class MatrixMkString[A:Manifest](mat: Exp[DeliteMultiArray[A]], rdel: Exp[String], cdel: Exp[String], dim: Int => Exp[Int], stringify: Seq[Exp[Int]] => Exp[String])(implicit ctx: SourceContext) extends DeliteOpSingleWithManifest[A,String](reifyEffectsHere{
    val rows = dim(0)
    val cols = dim(1)
    if (delite_less_than(rows, unit(1))) unit("[ ]")
    else if (delite_less_than(cols, unit(1))) unit("[ ]")
    else {
      val r = var_new(unit(0))
      val c = var_new(unit(0))
      val s = var_new(unit(""))

      def row_mkstring() {
        while (delite_less_than(c, delite_int_minus(cols, unit(1)))) {
          s = delite_string_concat(readVar(s), stringify(Seq(readVar(r),readVar(c))))
          s = delite_string_concat(readVar(s), cdel)
          c = delite_int_plus(readVar(c), unit(1))
        }
        s = delite_string_concat(readVar(s), stringify(Seq(readVar(r),readVar(c))))
      }
 
      while (delite_less_than(r, delite_int_minus(rows, unit(1)))) {
        row_mkstring()
        s = delite_string_concat(readVar(s), rdel)
        r = delite_int_plus(readVar(r), unit(1))
        c = unit(0)
      }
      row_mkstring()
      readVar(s)
    }
  }) { override def toString = "MatrixMkString(" + mat + ", " + rdel + ", " + cdel + ")" }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@ArrayMkString(m,d,l,a) => reflectPure(new {override val original = Some(f,e)} with ArrayMkString(f(m),f(d),l,a)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@MatrixMkString(m,r,c,d,a) => reflectPure(new {override val original = Some(f,e)} with MatrixMkString(f(m),f(r),f(c),d,a)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case Reflect(e@ArrayMkString(m,d,l,a), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with ArrayMkString(f(m),f(d),l,a)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@MatrixMkString(m,r,c,d,a), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with MatrixMkString(f(m),f(r),f(c),d,a)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

  // Override blocks for MkString to avoid cluttering IR printouts - can remove later if needed
  // TODO: Blocks still seem to be in highest IR "scope" - figure out if can be moved later?
  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: ArrayMkString[_] => Nil
    case op: MatrixMkString[_] => Nil
    case _ => super.blocks(e)
  }

  // --- Convenience methods for type checks
  def isLoopIndices[T](x: Exp[T]) = isSubtype(x.tp.erasure, classOf[LoopIndices])
  def isIndices[T](x: Exp[T]) = isSubtype(x.tp.erasure, classOf[Indices])

  def isMultiArrayImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayImpl[_]])
  def isMultiArrayViewImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayViewImpl[_]])
  def isMultiArrayBuffImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayViewImpl[_]])
  def isMultiArrayBuffViewImpl[T](x: Exp[T])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure, classOf[MultiArrayBuffViewImpl[_]])

  def asMultiArrayImpl[T](x: Exp[DeliteCollection[T]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[MultiArrayImpl[T]]]

  // --- DeliteCollection ops for MultiArray implementations
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isMultiArrayImpl(x)) implementer.implementSize(x.asInstanceOf[Exp[DeliteMultiArray[A]]])
    else super.dc_size(x)
  }

  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isMultiArrayImpl(x)) implementer.implementApply(x.asInstanceOf[Exp[DeliteMultiArray[A]]], loopindices_new(n, calcIndices(n, asMultiArrayImpl(x).dims)))
    else if (isDeliteArray(x) || isDeliteArrayBuffer(x)) {
      if (getLayout(x).nonEmpty)
        implementer.implementApply(x.asInstanceOf[Exp[DeliteMultiArray[A]]], loopindices_new(n, Seq(n)))
      else
        super.dc_apply(x,n)   // bit kludgey - this may be called before implementer is run, in which case use super
    }
    else super.dc_apply(x,n)
  }

  // Get symbol properties for MultiArray data field 
  def mdat(b: Block[Any]): SymbolProperties = mdat(b.res)
  def mdat(e: Exp[Any]): SymbolProperties = mdat(props(e))
  def mdat(p: SymbolProperties): SymbolProperties = p match {
    case (s: StructProperties) => s.child("data").get
    case (a: ArrayProperties) => a.child.get
    case _ => sys.error("Symbol properties " + makeString(p) + " has no data field")
  }

  trait MultiArrayImplementer extends TransformerBase {
    val IR: self.type

    // Get inner element type for given data structure type
    def dataTp[A,B:Manifest](x: Exp[A]): Manifest[B] = dataTp(x.tp)
    def dataTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp match {
      // Check if type is DeliteArray first to avoid issues with AOS - SOA in StructType unapplies
      case t if isDeliteArrayTpe(t) => t.typeArguments(0).asInstanceOf[Manifest[B]]
      case StructType(_,elems) => elems.find(_._1 == "data").getOrElse(
        throw new RuntimeException("Can't find data field for " + tp)
      )._2.typeArguments(0).asInstanceOf[Manifest[B]]
      case t if !t.typeArguments.isEmpty => t.typeArguments(0).asInstanceOf[Manifest[B]]
      case _ => sys.error("Cannot get data type of " + tp + " - type has no type arguments")
    }

    def keyTp[A,B:Manifest](x: Exp[A]): Manifest[B] = keyTp(x.tp)
    def keyTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp match {
      case StructType(_,elems) => elems.find(_._1 == "keys").getOrElse{
        throw new RuntimeException("Can't find keys field for " + tp)
      }._2.typeArguments(0).asInstanceOf[Manifest[B]]
      case _ => sys.error("Cannot get keys type of " + tp + " - not a struct type")
    }
    def valTp[A,B:Manifest](x: Exp[A]): Manifest[B] = valTp(x.tp)
    def valTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp match {
      case StructType(_,elems) => elems.find(_._1 == "values").getOrElse{
        throw new RuntimeException("Can't find keys field for " + tp)
      }._2.typeArguments(0).asInstanceOf[Manifest[B]]
      case _ => sys.error("Cannot get values type of " + tp + " - not a struct type")
    }

    // --- Type transformation
    /**
     * Transform saved manifests to match transformed IR
     * @param tp - element type manifest, to be transformed
     * @param p  - symbol properties matching type 
     */
    def ttype[A,B:Manifest](tp: Manifest[A], p: SymbolProperties): Manifest[B] = (tp,p) match { 
      case (StructType(_,elems), s: StructProperties) => 
        new RefinedManifest[Record] {
          def runtimeClass = classOf[Record]
          val fields = elems.map{f => f._1 -> ttype(f._2, s.child(f._1).get) }
        }.asInstanceOf[Manifest[B]]
      case (tp, a: ArrayProperties) => 
        val inner = ttype(tp.typeArguments(0), a.child.get)
        transformManifest(tp, a, inner).asInstanceOf[Manifest[B]]
      case (tp, a: ScalarProperties) => tp.asInstanceOf[Manifest[B]]
      case _ => sys.error("Don't know how to transform type " + tp + " with associated properties " + p)
    }

    // TODO: This can be generalized as part of an "AbstractNodeTransformer" trait if we ever need one
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case d: AbstractNode[_] if d.family == "multiarray" => Some(transformAbstract(s, d))
      case NestedAtomicWrite(_,trace,f) =>
        if ((f.isInstanceOf[AbstractNode[_]] && f.asInstanceOf[AbstractNode[_]].family == "multiarray") ||
            trace.map{case t: AbstractAtomicTracer => t.family == "multiarray"; case _ => false }.fold(false){_||_})
          Some(transformAbstract(s, d))
        else 
          None  
      case Reflect(f: AbstractNode[_],_,_) if f.family == "multiarray" => Some(transformAbstract(s, d))
      case Struct(tag, elems) if hasMultiArrayTpe(s.tp) => Some(transformAbstract(s, d))
      case Reflect(Struct(tag, elems),_,_) if hasMultiArrayTpe(s.tp) => Some(transformAbstract(s, d))
      
      case l: AbstractLoop[_] if hasMultiArrayTpe(s.tp) => transformLoop(s, l)(ttype(s.tp,props(s)), mtype(s.tp), ctx)
      case Reflect(l: AbstractLoop[_], _, _) if hasMultiArrayTpe(s.tp) => transformLoop(s,l)(ttype(s.tp,props(s)), mtype(s.tp), ctx)

      case c: DeliteOpCondition[_] if hasMultiArrayTpe(s.tp) => Some( transformCondition(s, c)(ttype(s.tp,props(s)), mtype(s.tp), ctx) )
      case Reflect(c: DeliteOpCondition[_], _,_) if hasMultiArrayTpe(s.tp) => Some(transformCondition(s, c)(ttype(s.tp, props(s)), mtype(s.tp), ctx) )

      case Reify(x,u,es) => Some(reflectPure(Reify(f(x),mapOver(f,u),f(es)))(f(x).tp,ctx))
      case _ => None
    }

    // Force body transformations and type changes when mirroring loops
    def transformLoop[A:Manifest,B:Manifest](s: Exp[B], d: AbstractLoop[_])(implicit ctx: SourceContext): Option[Exp[Any]] = d.body match {
      case re: DeliteReduceElem[_] =>
        println("Found reduce with output props " + makeString(props(s)))
        println(strDef(s))
        quoteCode(ctx).foreach{x => println(x)}

        val r = re.asInstanceOf[DeliteReduceElem[A]] 
        val mA = manifest[A]

        val size = f(d.size)
        val rV2 = (fresh[A],fresh[A])
        val func = f(r.func)(mA).asInstanceOf[Block[A]]
        val cond = r.cond.map{c => f(c).asInstanceOf[Block[Boolean]]}
        val zero = f(r.zero)(mA).asInstanceOf[Block[A]]
        val init = f(r.accInit)(mA).asInstanceOf[Block[A]]

        setProps(rV2._1, getProps(func))
        setProps(rV2._2, getProps(func))

        println("Transforming reduce with type " + manifest[A].toString)
        println(" and rV._1 " + rV2._1.tp + makeString(props(rV2._1)))

        val rFunc = withSubstScope(r.rV._1 -> rV2._1, r.rV._2 -> rV2._2) { f(r.rFunc).asInstanceOf[Block[A]] }

        val s2 = reflectPure(ReduceFactory.regen[A](d.v, rV2, size, func, rFunc, zero, cond))
        setProps(s2, getProps(rFunc))

        Some( s2 )
      case _ => None
    }

    def transformCondition[A:Manifest,B:Manifest](s: Exp[B], d: DeliteOpCondition[_])(implicit ctx: SourceContext): Exp[Any] = {
      val cond = f(d.cond)
      val thenp = f(d.thenp).asInstanceOf[Block[A]]
      val elsep = f(d.elsep).asInstanceOf[Block[A]]

      // HACK -- no other way to directly create a DeliteIfThenElse node from here
      val e = if (cond) thenp.res else elsep.res
      setProps(e, meet(getProps(thenp), getProps(elsep), MetaBranch))
      (e)
    }

    def transformAbstract[A](s: Exp[A], d: Def[A])(implicit ctx: SourceContext): Exp[Any] = transformMultiArrayDef(s, d)

    /**
     * Transforms abstract MultiArray IR nodes to concrete implementations using types like
     * DeliteArray, DeliteArrayBuffer, and anonymous Structs
     */
    def transformMultiArrayDef(s: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Exp[Any] = d match {
      // --- MultiMap stuff
      case DeliteMultiMapSize(dm) => implementMapSize(f(dm))(keyTp(f(dm)), valTp(f(dm)), ctx)
      case DeliteMultiMapGet(dm,k) => implementMapGet(f(dm),f(k))(keyTp(f(dm)), valTp(f(dm)), ctx)
      case DeliteMultiMapContains(dm,k) => implementMapContains(f(dm),f(k))(keyTp(f(dm)), valTp(f(dm)), ctx)
      case DeliteMultiMapKeys(dm) => implementMapKeys(f(dm))(keyTp(f(dm)), valTp(f(dm)), ctx)
      case DeliteMultiMapVals(dm) => implementMapVals(f(dm))(keyTp(f(dm)), valTp(f(dm)), ctx)
      case DeliteMultiMapFromArrays(k,v) => implementMapFromArrays(f(k),f(v))(dataTp(f(k)),dataTp(f(v)),ctx)

      // --- Nested Writes
      case op@NestedAtomicWrite(sym, trace, d) => transformNestedAtomicWrite(f(sym), trace.map{r => mirrorTrace(r,f)}, d)(dataTp(f(sym)), ctx)

      // --- Properties
      case DeliteMultiArrayRank(ma) => Const(rank(f(ma)))
      case DeliteMultiArrayDim(ma,i) => implementDim(f(ma),i)(dataTp(f(ma)), ctx)
      case DeliteMultiArraySize(ma) => implementSize(f(ma))(dataTp(f(ma)), ctx)

      // --- Array Constructors
      case op@DeliteMultiArrayNew(dims) => implementNew(f(dims), props(s))(ttype(op.mA, mdat(s)), ctx)
      case DeliteMultiArrayView(ma,o,t,d,ud) => implementView(f(ma),f(o),f(t),f(d),ud,props(s))(dataTp(f(ma)), ctx)

      // --- Single element ops
      case DeliteMultiArrayApply(ma,i) => implementApply(f(ma),f(i))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayUpdate(ma,i,x) => implementUpdate(f(ma),f(i),f(x))(dataTp(f(ma)), ctx)

      // --- Array permute / reshape
      case DeliteMultiArrayPermute(ma,config) => implementPermute(f(ma),config,props(s))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayReshape(ma,dims) => implementReshape(f(ma),f(dims),props(s))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayPermuteView(ma,config) => implementPermuteView(f(ma),config,props(s))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayReshapeView(ma,dims,_) => implementReshapeView(f(ma),f(dims),props(s))(dataTp(f(ma)), ctx)

      // --- Parallel ops
      case op@DeliteMultiArrayReadFile(path,dels,_) =>
        val body = f(op.body)
        implementReadFile(f(path), f(dels), body, op.v, op.i, op.rV, props(s))(mtype(body.tp), ctx)

      case op@DeliteMultiArrayFromFunction(dims,_) => 
        val body = f(op.body)
        implementFromFunction(f(dims),body,op.v,op.i,props(s))(mtype(body.tp), ctx)

      case op@DeliteMultiArrayMap(ma,_) => 
        val body = f(op.body)
        implementMap(f(ma),body,op.v,op.i,props(s))(dataTp(f(ma)),mtype(body.tp), ctx)

      case op@DeliteMultiArrayZipWith(ma,mb,_) => 
        val body = f(op.body)
        implementZipWith(f(ma),f(mb),body,op.v,op.i,props(s))(dataTp(f(ma)),dataTp(f(mb)),mtype(body.tp), ctx)

      case op@DeliteMultiArrayReduce(ma,_,z) => implementReduce(f(ma),op.lookup,op.body,f(z),op.v,op.i,op.rV,props(s))(dataTp(f(ma)), ctx)
      case op@DeliteMultiArrayFold(ma,_,z) => implementFold(f(ma),op.lookup,op.body,f(z),op.v,op.i,op.rV,props(s))(dataTp(f(ma)), ctx)

      case op@DeliteMultiArrayForeach(ma,_) => implementForeach(f(ma),f(op.body),op.v,op.i)(dataTp(f(ma)), ctx)
      case op@DeliteMultiArrayForIndices(ma,_) => implementForIndices(f(ma),f(op.body),op.v,op.i)(dataTp(f(ma)), ctx)
      case op@DeliteMultiArrayForShapeIndices(shape,_) => implementForShapeIndices(f(shape), f(op.body), op.v, op.i)(ctx)

      case op@DeliteMultiArrayNDMap(ma,_,_) => 
        val body = f(op.body)
        implementNDMap(f(ma),body,op.rV,props(s))(dataTp(f(ma)),dataTp(body.tp),ctx)
      case op@DeliteMultiArrayGroupBy(ma,_,_) => 
        val keyFunc = f(op.keyFunc)
        val valFunc = f(op.valFunc)
        implementGroupBy(f(ma), keyFunc, valFunc)(dataTp(f(ma)),mtype(keyFunc.tp),mtype(valFunc.tp),ctx)
      case op@DeliteMultiArrayGroupByReduce(ma,_,_,_) => 
        val keyFunc = f(op.keyFunc)
        val valFunc = f(op.valFunc)
        val redFunc = f(op.redFunc)
        implementGroupByReduce(f(ma),keyFunc,valFunc,redFunc,op.rV)(dataTp(f(ma)),mtype(keyFunc.tp),mtype(valFunc.tp),ctx)

      case op@DeliteMultiArrayFilterReduce(ma,z,_,_,a) => 
        implementFilterReduce(f(ma),f(z),op.filtFunc,op.redFunc,a,op.v,op.rV)(dataTp(f(ma)), ctx)

      // --- 1D Parallel ops
      case op@DeliteMultiArrayMapFilter(ma,_,_) => 
        val body = f(op.mapFunc)
        implementMapFilter(f(ma),body,f(op.filtFunc),op.v,op.i,props(s))(dataTp(f(ma)),mtype(body.tp),ctx)

      case op@DeliteMultiArrayFlatMap(ma,_) =>
        val body = f(op.body)
        implementFlatMap(f(ma),body,op.v,op.i,props(s))(dataTp(f(ma)),dataTp(body.tp),ctx)

      // --- Buffer ops
      case DeliteMultiArrayInsert(ma,x,i) => implementInsert(f(ma),f(i),f(x))(dataTp(f(ma)),ctx)
      case DeliteMultiArrayAppend(ma,x) => implementAppend(f(ma),f(x))(dataTp(f(ma)), ctx)
      case DeliteMultiArrayInsertAll(ma,x,a,i) => implementInsertAll(f(ma),a,f(i),f(x))(dataTp(f(ma)),ctx)
      case DeliteMultiArrayRemove(ma,a,s,l) => implementRemove(f(ma),a,f(s),f(l))(dataTp(f(ma)),ctx)

      // --- Misc
      case op@DeliteMultiArrayMkString(ma, dels, _) => 
        val body = op.body.map{f(_)}
        implementMkString(f(ma),f(dels),body,op.rV)(dataTp(f(ma)), ctx)
      
      case op@DeliteMultiArrayWriteFile(ma, dels, path, _) => 
        val body = f(op.body)
        implementWriteFile(f(ma), f(dels), f(path), body, op.v, op.i)(dataTp(ma), ctx)

      // --- 1D Ops
      case DeliteMultiArraySortIndices(len,i,body) => implementSortIndices(f(len),(f(i._1).asInstanceOf[Sym[Int]],f(i._2).asInstanceOf[Sym[Int]]),f(body),props(s))(ctx)
      case DeliteMultiArrayStringSplit(str,split,lim) => implementStringSplit(f(str),f(split),f(lim),props(s))(ctx)

      // --- 2D Ops
      case op@DeliteMatrixMultiply(lhs,rhs,_) => implementMatrixMultiply(f(lhs),f(rhs),f(op.defFunc), op.rM1, op.rM2)(op.mA,ctx)
      case op@DeliteMatrixVectorMultiply(mat,vec,_) => implementMatrixVectorMultiply(f(mat),f(vec),f(op.defFunc), op.rM, op.rV)(op.mA,ctx)

      // --- Structs
      case Struct(tag, elems) =>
        val fields = elems map {elem => elem._2 match {
          case Def(ReadVar(v)) => (elem._1, true, (x: Exp[_]) => f(elem._2))
          case _ => (elem._1, false, (x: Exp[_]) => f(elem._2))
        }}
        val out = record_new(fields)(ttype(s.tp, props(s)))
        elems foreach {elem => setField(out, getProps(f(elem._2)), elem._1) }
        copyMetadata(out, props(s))
        (out)

      // --- Wrapping
      case MultiArrayBuffify(ma) => implementBuffify(f(ma),props(s))(dataTp(f(ma)))
      case MultiArrayViewify(ma) => implementViewify(f(ma),props(s))(dataTp(f(ma)))

      case Reflect(d, u, es) => 
        val e = transformAbstract(s, d)
        transferMetadata(e, s, d)

        e match {
          // Exception added for MkString implementation returning a reify node
          // Reflect(Reify) shouldn't happen in general
          case Def(Reify(_,_,_)) => e 

          case Def(Reflect(d2, u2, es2)) => 
            val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(e.tp), ctx)
            setProps(out, getProps(e))
              
            // Also need to scrub intermediate Reflect node, otherwise its in the context list
            if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
            (out)
          case Def(d2) =>
            val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(e.tp), ctx)
            setProps(out, getProps(e))

            // Scrub intermediate def, otherwise its in the context list
            if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
            (out)
          case c: Const[_] => (c)   // This should never happen?
        }
    }

    override def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext) = d match {
      case DeliteMultiArrayNew(_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayView(_,_,_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayApply(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayPermute(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayReshape(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayPermuteView(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayReshapeView(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayFromFunction(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayMap(_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayZipWith(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayReduce(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayFold(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayNDMap(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayFilterReduce(_,_,_,_,_) => 
        println("Copying metadata from " + orig.tp + makeString(props(orig)))
        println("to " + sub.tp + makeString(props(sub)))
        println("init: " + makeString(initExp(sub)))
        copyMetadata(sub, props(orig))

      case DeliteMultiArrayMapFilter(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayFlatMap(_,_) => copyMetadata(sub,props(orig))

      case DeliteMultiArraySortIndices(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMultiArrayStringSplit(_,_,_) => copyMetadata(sub,props(orig))

      case DeliteMatrixMultiply(_,_,_) => copyMetadata(sub,props(orig))
      case DeliteMatrixVectorMultiply(_,_,_) => copyMetadata(sub,props(orig))

      case MultiArrayBuffify(_) => copyMetadata(sub,props(orig))
      case MultiArrayViewify(_) => copyMetadata(sub,props(orig))

      case _ => // Nothing
    }

    def transformManifest[T](t: Manifest[T], a: ArrayProperties, inner: Manifest[_]): Manifest[_] = {
      sys.error("Don't know how to transform type " + t + " with layout " + layout(a))
    }

    // --- Nested atomic updates
    def transformTracer[T:Manifest](ma: Exp[DeliteMultiArray[T]], t: MultiArrayTracer): List[AtomicTracer] = {
      sys.error("Don't know how to implement tracer for layout " + layout(ma))
    }

    // Requires that trace has already been transformed (has no MultiArrayTracers)
    def reconstructTrace(sym: Exp[Any], trace: List[AtomicTracer], p: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      if (trace.isEmpty) {sym}
      else (sym.tp, trace.head, p) match {
        // TODO: How to reconstruct variable trace? Is this needed, or does it happen implicitly?
        //case (_, VarTracer, _) =>    
        //  val childSym = readVar(sym)

        case (StructType(_,elems), StructTracer(index), symProps: StructProperties) =>
          val fType = elems.find(_._1 == index).getOrElse{throw new RuntimeException(index + " is not a field of type " + sym.tp)}._2
          val childSym = field(sym,index)(mtype(fType), ctx)
          setProps(childSym, symProps.child(index))
          reconstructTrace(childSym, trace.tail, symProps.child(index).get)

        case (tp, ArrayTracer(i), symProps: ArrayProperties) if isDeliteArrayTpe(tp) => 
          val childSym = darray_apply(sym.asInstanceOf[Exp[DeliteArray[Any]]], i)
          setProps(childSym, symProps.child)
          reconstructTrace(childSym, trace.tail, symProps.child.get) 

        case _ => sys.error("Error while reconstructing nested apply trace")
      }
    }

    // NOTE: This is a bit counterintuitive. For something like:
    // val x = Struct(data: Array2D[Int], ...)
    // x.data(1, 2) = 3
    // we actually now output something of the form:
    // val index = 1 * x.data.cols + 2   // Index pre-calculation
    // x.data.data(index) = 3            // Atomic write

    // Comment: In transforming nested atomic writes, we want to get the symbol representing the inner
    // data structure since we potentially need to pull dimensions/offsets/etc. from that. However, 
    // we also want to keep the metadata for all intermediate lookups around
    // Steps should be:
    //  1. Transform trace
    //    - requires reconstructing intermediate symbols for some MultiArray index calculations
    //  2. Reconstruct inner symbol 
    //  3. Transform inner def with reconstructed symbol
    //  4. reflect atomic write on transformed inner def
    def transformNestedAtomicWrite[A:Manifest](sym: Exp[Any], trace: List[AtomicTracer], d: AtomicWrite[A])(implicit ctx: SourceContext): Exp[Unit] = {
      /** 
       * @param sym     - most recently available/reconstructed symbol
       * @param done    - transformed part of trace
       * @param current - subset of done, trace from 'sym' to current tracer
       * @param remain  - untransformed section of trace
       */
      def transformTrace(sym: Exp[Any], done: List[AtomicTracer], current: List[AtomicTracer], remain: List[AtomicTracer]): (Exp[Any], List[AtomicTracer], List[AtomicTracer]) = {
        if (remain.isEmpty)
          (sym, done, current)
        else if (remain.indexWhere(t => t.isInstanceOf[MultiArrayTracer]) == -1)
          (sym, done ++ remain, current ++ remain)
        else (remain.head) match {
          case VarTracer => transformTrace(sym, done :+ VarTracer, current :+ VarTracer, remain.tail)
          case t@ArrayTracer(i) => transformTrace(sym, done :+ t, current :+ t, remain.tail)
          case t@StructTracer(f) => transformTrace(sym, done :+ t, current :+ t, remain.tail)
          case t: MultiArrayTracer => 
            val cur = reconstructTrace(sym, current, props(sym)).asInstanceOf[Exp[DeliteMultiArray[Any]]]
            val t2 = transformTracer(cur, t)
            transformTrace(cur, done ++ t2, t2, remain.tail)
          case x => sys.error("Error while transforming write tracer " + x)
        }
      }

      val (cur, full, partial) = transformTrace(sym, Nil, Nil, trace)
      val inner = reconstructTrace(cur,partial,props(cur))

      d match {
        case DeliteMultiArrayUpdate(_,i,x) => implementUpdate(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], i, x)
        case DeliteMultiArrayInsert(_,x,i) => implementInsert(inner.asInstanceOf[Exp[DeliteArray1D[A]]], i, x)
        case DeliteMultiArrayAppend(_,x) => implementAppend(inner.asInstanceOf[Exp[DeliteArray1D[A]]], x)
        case DeliteMultiArrayInsertAll(_,x,a,i) => implementInsertAll(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], a, i, x)
        case DeliteMultiArrayRemove(_,a,s,l) => implementRemove(inner.asInstanceOf[Exp[DeliteMultiArray[A]]], a, s, l)
        case _ => reflectWrite(sym)(NestedAtomicWrite(sym, full, d))
      }
    }

    // --- Properties
    def getStrides[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Seq[Exp[Int]] = {
      sys.error("Don't know how to implement strides for type " + ma.tp)
    }

    def implementStride[T:Manifest](ma: Exp[DeliteMultiArray[T]], d: Int)(implicit ctx: SourceContext): Exp[Int] = {
      sys.error("Don't know how to implement stride for type " + ma.tp)
    }

    def getDims[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Seq[Exp[Int]] = {
      sys.error("Don't know how to implement dims for type " + ma.tp)
    }

    def implementDim[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Int)(implicit ctx: SourceContext): Exp[Int] = {
      sys.error("Don't know how to implement dim for type " + ma.tp)
    }
    def implementSize[T:Manifest](ma: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[Int] = {
      sys.error("Don't know how to implement size for type " + ma.tp)
    }

    // --- Array constructors 
    def implementNew[A:Manifest](dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement new multiarray for target layout " + layout(out))
    }
    def implementView[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Rep[Int]], unitDims: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement view creation for layout " + layout(ma) + " and output layout " + layout(out))
    }

    // --- Single element operations
    def implementApply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices])(implicit ctx: SourceContext): Exp[A] = {
      sys.error("Don't know how to implement apply node for layout " + layout(ma)) 
    }
    def implementUpdate[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
      sys.error("Don't know how to implement update node for layout " + layout(ma))
    }

    // --- Array permute / reshaping
    def implementPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement permute operation for input layout " + layout(ma) + " and output layout " + layout(out))
    }
    def implementReshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement reshape operation for input layout " + layout(ma) + " and output layout " + layout(out))
    }
    def implementPermuteView[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement permute-view operation for input layout " + layout(ma) + " and output layout " + layout(out))
    }
    def implementReshapeView[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement reshape-view operation for input layout " + layout(ma) + " and output layout " + layout(out))
    }

    // --- Parallel ops
    def implementReadFile[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], body: Block[A], v: Sym[Int], i: Exp[LoopIndices], rV: Sym[String], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement file read for output layout " + layout(out))
    }
    def implementFromFunction[A:Manifest](dims: Seq[Exp[Int]], body: Block[A], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement map indices for output layout " + layout(out))
    }
    def implementMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement map for input layout " + layout(in) + " with output layout " + layout(out))
    }
    def implementZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], body: Block[R], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement map for layouts " + layout(inA) + " and " + layout(inB) + " for output layout " + layout(out))
    }
    def implementReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], lookup: Block[A], body: Block[A], zero: Exp[A], v: Sym[Int], i: Exp[LoopIndices], rV: (Sym[A], Sym[A]), out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)
      val (mirroredLookup, mirroredZero) = withSubstScope(i -> i2) { (f(lookup), f(zero)) }
      val rV2 = (fresh[A], fresh[A])
      setProps(rV2._1, getChild(in))
      setProps(rV2._2, getChild(in))
      val mirroredBody = withSubstScope(i -> i2, rV._1 -> rV2._1, rV._2 -> rV2._2) { f(body) }
      reflectPure(ReduceFactory(v, rV2, size, mirroredLookup, mirroredBody, mirroredZero)).withProps(props(mirroredBody))
    }
    def implementFold[A:Manifest](in: Exp[DeliteMultiArray[A]], lookup: Block[A], body: Block[A], zero: Exp[A], v: Sym[Int], i: Exp[LoopIndices], rV: (Sym[A], Sym[A]), out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)
      val (mirroredLookup, mirroredZero) = withSubstScope(i -> i2) { (f(lookup), f(zero)) }
      val rV2 = (fresh[A], fresh[A])
      setProps(rV2._1, getChild(in))
      setProps(rV2._2, getChild(in))
      val mirroredBody = withSubstScope(i -> i2, rV._1 -> rV2._1, rV._2 -> rV2._2) { f(body) }
      reflectPure(FoldFactory(v, rV2, size, mirroredLookup, mirroredBody, mirroredZero)).withProps(props(mirroredBody))
    }
    def implementForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)

      val mirroredBody = withSubstScope(i -> i2) { f(body) }
      reflectEffect(ForeachFactory(v, size, mirroredBody), summarizeEffects(mirroredBody).star andAlso Simple())
    }
    def implementForIndices[A:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
      val dims = getDims(in)
      val size = implementSize(in)
      val inds = calcIndices(v, dims)
      val i2 = loopindices_new(v, inds)

      val mirroredBody = withSubstScope(i -> i2) { f(body) }
      reflectEffect(ForeachFactory(v, size, mirroredBody), summarizeEffects(mirroredBody).star andAlso Simple())
    }
    def implementForShapeIndices(shape: Seq[Exp[Int]], body: Block[Unit], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
      val size = productTree(shape)
      val inds = calcIndices(v, shape)
      val i2 = loopindices_new(v, inds)

      val mirroredBody = withSubstScope(i -> i2){ f(body) }
      reflectEffect(ForeachFactory(v, size, mirroredBody), summarizeEffects(mirroredBody).star andAlso Simple())
    }

    def implementNDMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], body: Block[DeliteMultiArray[B]], rV: Sym[DeliteMultiArray[A]], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement ND map for input " + layout(in) + " and output " + layout(out))
    }
    def implementGroupBy[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], keyFunc: Block[K], valFunc: Block[V])(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement groupby for input " + layout(in))
    }
    def implementGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], keyFunc: Block[K], valFunc: Block[V], redFunc: Block[V], rV: (Sym[V], Sym[V]))(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement groupby-reduce for input " + layout(in))
    }

    def implementFilterReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], zero: Exp[DeliteMultiArray[A]], filter: Block[Boolean], reduce: Block[DeliteMultiArray[A]], axis: Int, v: Sym[Int], rV: (Sym[DeliteMultiArray[A]],Sym[DeliteMultiArray[A]]))(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement filter-reduce for input " + layout(in))
    }

    // --- 1D Parallel Ops
    def implementMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], mapFunc: Block[B], filtFunc: Block[Boolean], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement map-filter for input " + layout(in) + " and output " + layout(out))
    }
    def implementFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], body: Block[DeliteArray1D[B]], v: Sym[Int], i: Exp[LoopIndices], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement flatmap for input " + layout(in) + " and output " + layout(out))
    }

    // --- Buffer ops
    def implementInsert[A:Manifest](ma: Exp[DeliteArray1D[A]], i: Exp[Int], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
      sys.error("Don't know how to implement insert for layout " + layout(ma))
    }
    def implementAppend[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
      sys.error("Don't know how to implement append for layout " + layout(ma))
    }
    def implementInsertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, i: Exp[Int], x: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext): Exp[Unit] = {
      sys.error("Don't know how to implement insert all for lhs " + layout(ma) + " and rhs " + layout(x))
    }
    def implementRemove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
      sys.error("Don't know how to implement remove for layout " + layout(ma))
    }

    // --- Misc.
    private def stringify[A:Manifest](ma: Exp[DeliteMultiArray[A]], body: Option[Block[String]], rV: Sym[A], indices: Seq[Exp[Int]])(implicit ctx: SourceContext): Exp[String] = body match {
      case Some(body) => 
        val rV2 = implementApply(ma, indices_new(indices))
        val mirroredBody = withSubstScope(rV -> rV2){ f(body) }
        getBlockResult(mirroredBody)

      case None =>
        delite_stringify(implementApply(ma, indices_new(indices)))
    }

    private def array_mkstring[A:Manifest](arr: Exp[DeliteMultiArray[A]], del: Exp[String], stringify: Seq[Exp[Int]] => Exp[String])(implicit ctx: SourceContext): Exp[String] 
      = ArrayMkString(arr, del, () => implementSize(arr), {i: Exp[Int] => stringify(Seq(i))} )
    private def matrix_mkstring[A:Manifest](mat: Exp[DeliteMultiArray[A]], rdel: Exp[String], cdel: Exp[String], stringify: Seq[Exp[Int]] => Exp[String])(implicit ctx: SourceContext): Exp[String]
      = MatrixMkString(mat, rdel, cdel, {i: Int => implementDim(mat, i)}, {i: Seq[Exp[Int]] => stringify(i) })

    /**
     * Create a string representation of the input MultiArray
     * If no body is supplied, delite_stringify is used to turn elements to strings
     * @param ma   - input MultiArray
     * @param dels - delimeters separating elements, dimensions
     * @param body - optional reified mapping function from string to element type
     * @param rV   - original bound symbol used to reify map body
     */
    def implementMkString[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], body: Option[Block[String]], rV: Sym[A])(implicit ctx: SourceContext): Exp[String] = {
      quoteCode(ctx).foreach{code => println(code)}
      println("Implementing mkString for " + ma.tp + makeString(props(ma)))

      if (rank(ma) == 1)      array_mkstring(ma, dels(0), {seq => stringify(ma, body, rV, seq)})
      else if (rank(ma) == 2) matrix_mkstring(ma, dels(0), dels(1), {seq => stringify(ma, body, rV, seq)})
      else sys.error("Don't know how to implement MkString for layout " + layout(ma))
    }

    def implementWriteFile[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], path: Exp[String], body: Block[String], v: Sym[Int], i: Exp[LoopIndices])(implicit ctx: SourceContext): Exp[Unit] = {
      sys.error("Don't know how to implement file write for layout " + layout(ma))
    }

    // --- 1D Ops
    def implementSortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement sortIndices for output layout " + layout(out))
    }
    def implementStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int], out: SymbolProperties)(implicit ctx: SourceContext): Exp[Any] = { 
      sys.error("Don't know how to implement string split for output layout " + layout(out))
    }

    // --- 2D Ops
    def implementMatrixMultiply[A:Manifest](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]], default: Block[DeliteArray2D[A]], rM1: Sym[DeliteArray2D[A]], rM2: Sym[DeliteArray2D[A]])(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement matrix multiply for layouts " + layout(lhs) + ", " + layout(rhs))
    }
    def implementMatrixVectorMultiply[A:Manifest](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]], default: Block[DeliteArray1D[A]], rM: Sym[DeliteArray2D[A]], rV: Sym[DeliteArray1D[A]])(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement matrix-vector multiply for layotus " + layout(mat) + ", " + layout(vec))
    }

    // --- Wrap
    def implementBuffify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties): Exp[Any] = {
      sys.error("Don't know how to implement buffify for input layout " + layout(ma) + " and output layout " + layout(out))
    }
    def implementViewify[A:Manifest](ma: Exp[DeliteMultiArray[A]], out: SymbolProperties): Exp[Any] = {
      sys.error("Don't know how to implement viewify for input layout " + layout(ma) + " and output layout " + layout(out))
    }

    // --- Map stuff
    def implementMapSize[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Exp[Int] = { 
      dmap_size(dm.asDeliteMap) withProps getField(dm, "size")
    }
    def implementMapGet[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]], key: Exp[K])(implicit ctx: SourceContext): Exp[V] = { 
      dmap_get(dm.asDeliteMap, key) withProps getField(dm, "values").flatMap{getChild(_)}
    }
    def implementMapContains[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]], key: Exp[K])(implicit ctx: SourceContext): Exp[Boolean] = { 
      dmap_contains(dm.asDeliteMap, key)
    }
    def implementMapKeys[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Exp[Any] = { 
      dmap_keys(dm.asDeliteMap) withProps getField(dm, "keys")
    }
    def implementMapVals[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Exp[Any] = {
      dmap_values(dm.asDeliteMap) withProps getField(dm, "values")
    }
    def implementMapFromArrays[K:Manifest,V:Manifest](keys: Exp[DeliteArray1D[K]], vals: Exp[DeliteArray1D[V]])(implicit ctx: SourceContext): Exp[Any] = {
      sys.error("Don't know how to implement map from array layouts " + layout(keys) + " and " + layout(vals))   
    }
  }
}
