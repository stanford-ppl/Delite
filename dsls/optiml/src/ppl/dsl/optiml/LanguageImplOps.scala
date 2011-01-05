package ppl.dsl.optiml

import scala.virtualization.lms.common.{ScalaOpsPkg, Base}
import datastruct.scala.{Vector,Matrix}

trait LanguageImplOps { this: OptiML =>
  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A]

  def optiml_vectordistance_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int]): Rep[A]
  def optiml_matrixdistance_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int]): Rep[A]
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: OptiML =>

  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A] = {

    var delta = unit(scala.Math.MAX_DOUBLE)
    var prev = unit(null).asInstanceOfL[A]
    var next = x
    var iter = unit(0)

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.cloneL()
      else
        prev = next

//      try{
        next = block(next)
//      }
//      catch{
//        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
//      }
      iter += 1
      delta = diff(next, prev)
      println("(" + delta + ")")
    }

      if (iter == max_iter){
        //throw new ConvergenceException("Maximum iterations exceeded")
        println("Maximum iterations exceeded")
        returnL()
      }

    next
  }


  // TODO: we should consolidate these into one implementation

  def optiml_vectordistance_impl[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int]) = {
    // match, exceptions are not lifted yet
//    metric match {
//     case ABS_DISTANCE => absdist(v1, v2)
//     case EUC_DISTANCE => eucdist(v1, v2)
//     case _ => throw new UnsupportedOperationException("unknown dist metric selected")
//    }
    if (metric == ABS_DISTANCE) {
      vector_absdist(v1, v2)
    }
    else if (metric == EUC_DISTANCE) {
      vector_eucdist(v1, v2)
    }
    else {
      println("error: unknown dist metric selected")
      exit(0)
    }
  }

  private def vector_absdist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = (v1-v2).abs.sum
  //private def eucdist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = Math.sqrt(((v1-v2) map {e => e*e}).sum)
  private def vector_eucdist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = {println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED"); v1(0)}//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
  
  def optiml_matrixdistance_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int]) = {
    // match, exceptions are not lifted yet
//    metric match {
//     case ABS_DISTANCE => absdist(v1, v2)
//     case EUC_DISTANCE => eucdist(v1, v2)
//     case _ => throw new UnsupportedOperationException("unknown dist metric selected")
//    }
    if (metric == ABS_DISTANCE) {
      matrix_absdist(m1, m2)
    }
    else if (metric == EUC_DISTANCE) {
      matrix_eucdist(m1, m2)
    }
    else {
      println("error: unknown dist metric selected")
      exit(0)
    }
  }

  private def matrix_absdist[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = (m1-m2).abs.sum
  private def matrix_eucdist[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = {println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED"); m1(0,0)}//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
}
