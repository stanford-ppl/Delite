package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.transform._

// you can pick and choose your overrides, these are provided for convenience
trait DeliteAllOverridesExp extends DeliteIfThenElseExp /*with DeliteOpMap*/ with DeliteWhileExp {
  this: DeliteOpsExp =>
}

trait DeliteScalaGenAllOverrides extends DeliteScalaGenVariables with DeliteScalaGenIfThenElse /*with DeliteScalaGenRange*/ with DeliteScalaGenWhile  {
  val IR: DeliteAllOverridesExp
    
  /**
   * Avoid remapping Nothing to generated.scala.Nothing
   */
  override def remap[A](m: Manifest[A]): String = {     
    val nothing = manifest[Nothing]
    m match {
      case `nothing` => "Nothing"
      case _ => super.remap(m)
    }
  }

  override def emitFileHeader() {
    // This override was moved here from LMS,
    // because it was breaking stuff there (not sure
    // this is the best place to put it/TODO)
    stream.println("package generated." + this.toString)
  }
}

trait DeliteCudaGenAllOverrides extends DeliteCudaGenVariables with DeliteCudaGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteCudaGenWhile {
  val IR: DeliteApplication with DeliteAllOverridesExp
}

trait DeliteOpenCLGenAllOverrides extends DeliteOpenCLGenVariables with DeliteOpenCLGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteOpenCLGenWhile {
  val IR: DeliteApplication with DeliteAllOverridesExp
}

trait DeliteCGenAllOverrides extends DeliteCGenVariables with DeliteCGenIfThenElse /*with DeliteCGenRange*/ with DeliteCGenWhile  {
  val IR: DeliteApplication with DeliteAllOverridesExp
}
