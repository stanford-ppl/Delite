package ppl.dsl.optiml

import scala.virtualization.lms.common.Base


trait VectorOpsInlined extends Base

trait VectorOpsInlinedExp extends VectorOpsInlined {
  //////////////////////////////////////////////////////////
  // (so far failed) attempts at construction-time expansion

  // TODO: this does not work because vector_new_impl actually needs the type param A also
  //object VectorNew extends ApplyExtractor[(Boolean,Int),Vector[_]](vector_new_impl)

  //def vector_new[A : Manifest](isRow: Exp[Boolean], len: Exp[Int]) : Exp[Vector[A]] = VectorNew(ETuple2(isRow, len)).asInstanceOf[Exp[Vector[A]]]
  //def vector_new[A : Manifest](isRow: Exp[Boolean], len: Exp[Int]) : Exp[Vector[A]] = new ApplyExtractor[(Boolean,Int),Vector[A]](vector_new_impl[A])(toAtom(ETuple2(isRow,len)))

  // this works
  //object VectorZeros extends ApplyExtractor[Int,Vector[Double]](vector_obj_zeros_impl)
  //def vector_obj_zeros(len: Exp[Int]) = VectorZeros(len)

}