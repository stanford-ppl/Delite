package ppl.dsl.optiml.vector

import ppl.dsl.optiml.{Vector,Matrix,IndexVector}
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optiml.{OptiMLLift, OptiMLCompiler, OptiML}

trait VectorImplOps { this: OptiML =>
  def vector_apply_indices_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], indices: Interface[IndexVector])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_find_override_impl[A:Manifest](v: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[IndexVector]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  //////////////////////////
  // kernel implementations

  def vector_apply_indices_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], indices: Interface[IndexVector])(implicit b: VectorBuilder[A,VA]) = {
    val resultOut = b.alloc(indices.length, v.isRow)
    val result = b.toIntf(resultOut)    
    for (i <- 0 until indices.length){
      result(i) = v(indices(i))
    }
    resultOut.unsafeImmutable    
  }
  
  def vector_find_override_impl[A:Manifest](v: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]) = {
    val indices = IndexVector(0, true)
    for (i <- 0 until v.length) {
      if (pred(v(i))) indices += i
    }
    indices.unsafeImmutable.asInstanceOf[Rep[IndexVector]]
  }
}