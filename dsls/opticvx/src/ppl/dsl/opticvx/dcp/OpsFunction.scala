package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsFunction extends DCPOpsGlobal {
  
  /*
    val square = {
      val x = cvxexpr()
      val t = cvxexpr()
      cvxfun(
        params(),
        args(scalar -> x),
        sign(positive),
        tonicity(x.sign),
        vexity(positive),
        over(scalar -> t),
        let(),
        where(
          in_secondorder_cone(cat(2*x, t-1), t+1)
        ),
        value(t)
      )
    }
  */

  /*
    cvxfun0(() => args()

    val square = {
      val n = cvxfunparam()
      val x = cvxfunarg()
      val t = cvxfunarg()
      cvxfun(
        params(),
        args(scalar -> x),
        sign(positive),
        tonicity(x.sign),
        vexity(positive),
        over(scalar -> t),
        let(),
        where(
          in_secondorder_cone(cat(2*x, t-1), t+1)
        ),
        value(t)
      )
    }
  */

  case class CvxFunExpr(val fx: Function)
  case class CvxFunConstraint(val fx: Function) {
    if(!(fx.isIndicator)) throw new IRValidationException()
  }

  class CvxFunParamSymbol {
    protected[DCPOpsFunction] var boundparam: IRPoly = null
  }
  class CvxFunExprSymbol {
    protected[DCPOpsFunction] var boundexpr: CvxFunExpr = null
  }

  class CvxFunParams(val params: CvxFunParamSymbol) 

  class CvxFunArgs(val args: Seq[CvxFunArgBinding])
  class CvxFunArgBinding(val size: IRPoly, val symbol: CvxFunExprSymbol)

  class CvxFunSign(val sign: SignumPoly)

  class CvxFunTonicity(val tonicity: Seq[SignumPoly])

  class CvxFunVexity(val vexity: SignumPoly)

  class CvxFunOver(val vars: Seq[CvxFunOverBinding])
  class CvxFunOverBinding(val size: IRPoly, val symbol: CvxFunExprSymbol)

  class CvxFunLet(val exprs: Seq[CvxFunLetBinding])
  class CvxFunLetBinding(val expr: CvxFunExpr, val symbol: CvxFunExprSymbol)

  class CvxFunWhere(val constraints: Seq[CvxFunConstraint])

  class CvxFunValue(val expr: CvxFunExpr)
}