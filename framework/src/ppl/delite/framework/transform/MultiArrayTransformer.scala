package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._

import ppl.delite.framework.analysis._
import ppl.delite.framework.visit._

/**** Mix in MultiArray implementation Exp traits here ****/
trait MultiArrayTransform extends DeliteVisit with MultiArrayImplExp
  with FlatArrayImplExp with BLASImplExp { 

  self: DeliteFileReaderOpsExp => 

  /**** Mix in MultiArray transformer traits here ****/
  trait MultiArrayTransformer extends MultiArrayImplementer with FlatArrayImplementer with BLASImplementer { 
    override val name = "MultiArray Transformer"
    override val printAfter = true 
  }

  override val implementer = new MultiArrayTransformer{val IR: self.type = self}
  prependVisitor(implementer)

  private val l = new LayoutAnalyzer{val IR: self.type = self}
  prependVisitor(l)

  private val w = new MultiArrayWrapTransformer{
    val IR: self.type = self
    override val printBefore = true
  }
  prependVisitor(w)

  private val c = new RankChecker{val IR: self.type = self}
  prependVisitor(c)

  private val r = new RankAnalyzer{val IR: self.type = self}
  prependVisitor(r)
}

/**** Mix in MultiArray codegen traits here ****/
trait ScalaGenMultiArray extends ScalaGenBLASImpl
trait CudaGenMultiArray extends CudaGenBLASImpl
trait OpenCLGenMultiArray extends OpenCLGenBLASImpl
trait CGenMultiArray extends CGenBLASImpl
