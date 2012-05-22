package ppl.delite.framework.extern.codegen.scala

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.GenericGenExternal
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

trait ScalaGenExternalBase extends GenericGenExternal with ScalaGenEffect {
  val IR: DeliteOpsExp
  import IR._

  // TODO: what about jvm -> jvm external lib? then we don't need any of this.. maybe this should be ScalaJNIGenExternalBase?
  
  override def libInterfaceHdr(lib: ExternalLibrary) = """
package %s
object %s {
System.load("%s")
""".format("generated.scala", lib.name, new File(libDir, "/" + lib.name + ".so"))

  override def libInterfaceFtr(lib: ExternalLibrary) = "}"

  val hdrExt = "scala"
  
  /////////////////
  // implementation
    
  def emitMethodCall(sym: Sym[Any], e: DeliteOpExternal[_], lib: ExternalLibrary, args: List[String]) = {
    stream.println("val " + quote(sym) + " = {")
    emitBlock(e.allocVal) 
    stream.println(quote(getBlockResult(e.allocVal)))
    stream.println("}")                    
    stream.println(lib.name + "." + e.funcName + "(" + (args mkString ",") + ")")    
  }
  
  def emitInterfaceAndMethod(lib: ExternalLibrary, funcName: String, scalaArgs: List[String], cArgs: List[String], body: String) = {
    // generated.scala is hardcoded as a package name in many places; should this be more flexible? does it matter?
    super.emitInterfaceAndMethod(lib, funcName,
      "def " + funcName + "(" + (scalaArgs mkString ",") + ")",
      "JNIEXPORT void JNICALL Java_generated_scala_" + lib.name + "_00024_"+funcName+"(JNIEnv *env, jobject obj, " + (cArgs mkString ",") + ")"+body
    )
  }
    
  // one per method into shared file <libName>.scala
  override def emitMethod(lib: ExternalLibrary, method: String) {
    val s = interfaceStreams.getOrElse(lib, throw new IllegalArgumentException("tried to emit JNI header without an open interface file"))
    s.println("@native")
    s.println(method)
    s.println("")
  }
}
