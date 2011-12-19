package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object TrainSetTestRunner extends OptiMLApplicationRunner with TrainSetTest

trait TrainSetTest extends OptiMLApplication {

  def main() = {

    val mat1 = Matrix.onesf(10,10)
    //val vec1 = Vector.onesf(10)
    //val label1 = Labels(vec1)
    //val ts1 = TrainingSet(mat1,label1)
    val out = mat1 + mat1
    //val out = ts1 + ts1.t
    out.pprint
  }
}
