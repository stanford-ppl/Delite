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
      //println("(" + delta + ")")
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
    if (metric == ABS) {
      (v1-v2).abs.sum
    }
    else if (metric == EUC) {
      //Math.sqrt(((v1-v2) mmap {e => e*e}).sum)
      println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")
      v1(0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
    }
    else if (metric == SQUARE) {
      ((v1 - v2) mmap { e => e*e}).sum
    }
    else {
      println("error: unknown dist metric selected")
      exit(0)
    }
  }

  def optiml_matrixdistance_impl[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int]) = {
    // match, exceptions are not lifted yet
//    metric match {
//     case ABS_DISTANCE => absdist(v1, v2)
//     case EUC_DISTANCE => eucdist(v1, v2)
//     case _ => throw new UnsupportedOperationException("unknown dist metric selected")
//    }
    if (metric == ABS) {
      (m1-m2).abs.sum
    }
    else if (metric == EUC) {
      println("NOT IMPLEMENTED YET -- SHOULD NOT BE CALLED")
      m1(0,0)//External[Rep[A]]("throw new UnsupportedOperationException('not implemented yet')")
    }
    else if (metric == SQUARE) {
      ((m1 - m2) mmap { e => e*e}).sum
    }
    else {
      println("error: unknown dist metric selected")
      exit(0)
    }
  }
}
