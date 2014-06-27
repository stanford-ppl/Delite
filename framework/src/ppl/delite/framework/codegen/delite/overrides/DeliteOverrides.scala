package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.ScalaGenDeliteStruct

trait DeliteAllOverridesExp extends DeliteIfThenElseExp /*with DeliteOpMap*/ with DeliteWhileExp {
  this: DeliteOpsExp =>
}

trait DeliteScalaGenAllOverrides extends DeliteScalaGenVariables with DeliteScalaGenIfThenElse /*with DeliteScalaGenRange*/ with DeliteScalaGenWhile with ScalaGenDeliteStruct {
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
    stream.println("package " + packageName)
  }
}

trait DeliteCudaGenAllOverrides extends DeliteCudaGenVariables with DeliteCudaGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteCudaGenWhile {
  val IR: DeliteAllOverridesExp
}

trait DeliteOpenCLGenAllOverrides extends DeliteOpenCLGenVariables with DeliteOpenCLGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteOpenCLGenWhile {
  val IR: DeliteAllOverridesExp
}

trait DeliteCGenAllOverrides extends DeliteCGenVariables with DeliteCGenIfThenElse /*with DeliteCGenRange*/ with DeliteCGenWhile  {
  val IR: DeliteAllOverridesExp
}
