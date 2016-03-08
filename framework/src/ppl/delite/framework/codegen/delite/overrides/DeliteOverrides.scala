package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.ScalaGenDeliteStruct
import ppl.delite.framework.Config
import scala.virtualization.lms.internal.CLikeCodegen
import scala.virtualization.lms.internal.DotCodegen

trait DeliteAllOverridesExp extends DeliteIfThenElseExp /*with DeliteOpMap*/ with DeliteWhileExp {
  this: DeliteOpsExp =>
}

trait DeliteScalaGenAllOverrides extends DeliteScalaGenVariables with DeliteScalaGenIfThenElse /*with DeliteScalaGenRange*/ with DeliteScalaGenWhile with ScalaGenDeliteStruct {
  val IR: DeliteAllOverridesExp
    
  /**
   * Avoid remapping Nothing to generated.scala.Nothing
   */
  override def remap[A](m: Manifest[A]): String = m.toString match {
    case "Nothing" => "Nothing"
    case "Int" if Config.intSize == "long" => "Long"
    //case "Int" if Config.intSize == "short" => "Short"
    case _ => super.remap(m)
  }

  override def emitFileHeader() {
    stream.println("package " + packageName)
  }
}

trait CLikeTypeOverrides extends CLikeCodegen {
  override def remap[A](m: Manifest[A]): String = m.toString match {
    case "Int" if Config.intSize == "long" => "int64_t"
    //case "Int" if Config.intSize == "short" => "int16_t"
    case _ => super.remap(m)
  }
}

trait DeliteCudaGenAllOverrides extends DeliteCudaGenVariables with DeliteCudaGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteCudaGenWhile {
  val IR: DeliteAllOverridesExp

  override def initializeGenerator(buildDir:String): Unit = {
    super.initializeGenerator(buildDir)
    headerStream.println("#include \"DeliteCuda.h\"")
  }
}

trait DeliteOpenCLGenAllOverrides extends DeliteOpenCLGenVariables with DeliteOpenCLGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteOpenCLGenWhile {
  val IR: DeliteAllOverridesExp

  override def initializeGenerator(buildDir:String): Unit = {
    super.initializeGenerator(buildDir)
    headerStream.println("#include \"DeliteOpenCL.h\"")
  }
}

trait DeliteDotGenAllOverrides extends DotCodegen {
  val IR: DeliteAllOverridesExp

  override def initializeGenerator(buildDir:String): Unit = {
    super.initializeGenerator(buildDir)
  }
}


trait DeliteCGenAllOverrides extends DeliteCGenVariables with DeliteCGenIfThenElse /*with DeliteCGenRange*/ with DeliteCGenWhile  {
  val IR: DeliteAllOverridesExp

  override def initializeGenerator(buildDir:String): Unit = {
    super.initializeGenerator(buildDir)
    headerStream.println("#include \"DeliteCpp.h\"")
  }
}
