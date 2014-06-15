import ppl.dsl.optiml._
import scala.virtualization.lms.common.Record
import scala.reflect.SourceContext

object CGCompiler extends OptiMLApplicationRunner with CG

trait CG extends OptiMLApplication { 

  def main() = {
    val A = DenseMatrix.ones(10,10) //readMatrix(args(0))
    val b = DenseVector.ones(10).t //readVector(args(1)).t
    val expected_result = DenseVector.ones(10).t //readVector(args(2)).t
    val x0 = DenseVector.zeros(A.numCols).t

    //val r0 = b - A*x0
    val r0 = b - A.mapRowsToVector(v => (v*x0).sum)
    val p0 = r0

    implicit def diffCG(t1: Rep[(DenseVector[Double],DenseVector[Double],DenseVector[Double])],
                        t2: Rep[(DenseVector[Double],DenseVector[Double],DenseVector[Double])]) = {
      val (x, r, p) = t3(t2)
      sqrt(r *:* r)
    }

    val result = untilconverged(make_tuple3(x0, r0, p0),0.0,max_iter=100) { cur =>
      val (x, r, p) = t3(cur)

      //val Ap = A * p
      val Ap = A.mapRowsToVector(v => (v * p).sum)
      val alpha = (r *:* r) / (p *:* Ap)
      val x_next = x + alpha * p
      val r_next = r - alpha * Ap
      val beta = (r_next *:* r_next) / (r *:* r)
      val p_next = r_next + beta*p

      make_tuple3(x_next, r_next, p_next)
    }

    val (x_soln, r_soln, p_soln) = t3(result)

    val err = x_soln - expected_result
    println(sqrt(err *:* err))
  }

}
