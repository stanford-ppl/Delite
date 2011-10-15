package ppl.dsl.tests.GPU_tests

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication


object KM_map1Runner extends OptiMLApplicationRunner with KM_map1

trait KM_map1 extends OptiMLApplication {

  def main() {
    println("Kmeans map1 generation")
    val vec1 = DenseVector[Int](10)
    val x = Matrix[Double](10,10)
    val mu = Matrix[Double](10,10)
    val m = vec1(1)
    val c = (0::m){e => findNearestCluster(x(e), mu)}
    //val c = vec1.map{e => findNearestCluster(x(e), mu)}

    println(c)
  }

  def findNearestCluster( x_i: Rep[DenseVector[Double]], mu: Rep[Matrix[Double]] ) : Rep[Int] = {
    var min_d = unit(scala.Double.PositiveInfinity)
    var min_j = unit(-1)
    var j = unit(0)

    while( j < mu.numRows ){
      val dist = sum(0, x_i.length){ e => (x_i(e)-mu(j,e))*(x_i(e)-mu(j,e)) }
      if (dist < min_d){
        min_d = dist
        min_j = j
      }
      j += 1
    }
    return min_j
  }
}