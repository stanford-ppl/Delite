package ppl.apps.interop

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

/**
 * For now, just assume the OptiQL input data is there. Eventually, extend this example to actually call OptiQL.  
 */

object CustomerPricingRunner extends OptiMLApplicationRunner with CustomerPricing

trait CustomerPricing extends OptiMLApplication {
  def main() = {
    type Record = (Int,Double,Double)
    //val orderData = Array[Record]((1,2,3),(1,2,4),(3,4,3),(1,4,3))
    val orderData = Vector[Record]((1,2.,3.),(1,2.,4.),(3,4.,3.),(1,4.,3.))
    
    //val data = Matrix(orderData map { t => Array(t._1,t._2,t._3)})
    val data = Matrix(orderData map { t => Vector[Double](t._1,t._2,t._3) })
    val x = data.t.sliceRows(0,1).t
    val y = data.getCol(2).Clone
    val theta = linreg.weighted(x,y)
    println("theta: ")
    theta.pprint
  }
}

// object CustomerPricingApp {
//   // def OptiQL(b: => Unit) =
//   //   new Scope(OptiQLApplication, OptiQLApplicationRunner, Unit)(b)
//   def OptiML(b: => Unit) =
//     new Scope(OptiMLApplication, OptiMLApplicationRunner, Unit)(b)
//     
//   def main(args: Array[String]) = {
//     type Record = (Int,Double,Double)
//     // val orderData = DeliteRef(Array[Record]())
//     // val theta = DeliteRef(Array[Double]())
// 
//     // 
//     // OptiQL {
//     //   // customers: Array[Customer]
//     //   // orders: Array[Order]
//     //   val orders = customers Join(orders)
//     //     WhereEq(_.c_custkey, _.o_custkey)
//     //     Select((c,o) => new Result {
//     //       val nationKey = c.c_nationkey
//     //       val acctBalance = c.c_acctbal
//     //       val price = o.o_totalprice
//     //     })
//     //   orderData.set(orders)
//     // }
//     
//     val orderData = Array[Record]((1,2.,3.),(1,2.,4.),(3,4.,3.),(1,4.,3.))
//     
//     OptiML {
//       // run linear regression on price
//       val data = Matrix(Vector(orderData) map (t => Vector[Double](t._1,t._2,t._3)))
//       val x = data.sliceCols(0,1)
//       val y = data.getCol(2)
//       //theta.set(linreg.weighted(x,y).toArray)
//       val theta = linreg.weighted(x,y)
//       println("theta: " )
//       theta.pprint
//     }
//     
//     //println("theta: " + theta.get)
//   }
// }