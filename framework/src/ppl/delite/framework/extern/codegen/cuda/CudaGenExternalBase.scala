package ppl.delite.framework.extern.codegen.cuda

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.extern.codegen.GenericGenExternalBase
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

trait CudaGenExternalBase extends GenericGenExternalBase with CudaGenBase {
  val IR: DeliteOpsExp
  import IR._

  val hdrExt = "h"
  // TODO: if we have multiple CUDA libraries, they have to share this header file, so the logic in GenericGenExternal needs to accomodate that..
  override def hdrName(lib: ExternalLibrary) = "library" // runtime convention with GPUExecutableGenerator (better way to do this?)
  
  /////////////////
  // implementation
    
  def emitMethodCall(sym: Sym[Any], e: DeliteOpExternal[_], lib: ExternalLibrary, args: List[String])(implicit stream: PrintWriter) = {
    stream.println(lib.name + "." + e.funcName + "(" + (args mkString ",") + ")")    
  }
  
  def emitInterfaceAndMethod(lib: ExternalLibrary, funcName: String, args: List[String], body: String) = {
    val funcSignature = "void " + funcName + "(" + (args mkString ",") + ")"
    super.emitInterfaceAndMethod(lib, funcName,
      funcSignature + ";",
      funcSignature + body
    )
  }
     
}