package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._

import ppl.delite.framework.analysis._
import ppl.delite.framework.DeliteApplication

/**** Mix in MultiArray implementation Exp traits here ****/
trait MultiArrayTransform extends DeliteApplication with MultiArrayImplExp
  with FlatArrayImplExp { self =>

  /**** Mix in MultiArray transformer traits here ****/
  trait MultiArrayTransformer extends MultiArrayImplementer with FlatArrayImplementer { 
    override val name = "MultiArray Transformer"
    override val debugMode = false 
  }

  override val implementer = new MultiArrayTransformer{val IR: self.type = self}
  prependVisitor(implementer)

  private val l = new LayoutAnalyzer{val IR: self.type = self}
  prependVisitor(l)

  private val w = new MultiArrayWrapTransformer{val IR: self.type = self}
  prependVisitor(w)

  private val c = new RankChecker{val IR: self.type = self}
  prependVisitor(c)

  private val r = new RankAnalyzer{val IR: self.type = self}
  prependVisitor(r)
}
