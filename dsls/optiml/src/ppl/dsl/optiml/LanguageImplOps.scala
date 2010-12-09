package ppl.dsl.optiml

import scala.virtualization.lms.common.{ScalaOpsPkg, Base}

trait LanguageImplOps { this: Base with ArithImplicits =>

  def optiml_sum_impl[A:Manifest:ArithOps](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]) : Rep[A]

}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: ScalaOpsPkg with ArithImplicits with VectorOps =>

  def optiml_sum_impl[A](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A])(implicit mA: Manifest[A], ops: ArithOps[A]) = {
    val numProcs = 1 //Config.CPUThreadNum
    val coll = Vector.range(0,numProcs).map(j => {
      val chunk_st = start + end*j/numProcs
      val chunk_en = start + end*(j+1)/numProcs
      var acc = block(chunk_st)
      var i = chunk_st+1
      while(i < chunk_en) {
        acc = ops.+=(acc, block(i))
        i += 1
      }
      acc
    })

    coll.sum
  }
}
