package ppl.delite.framework.extern

import scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer}
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

trait DeliteGenExternal extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  ///////////////////
  // generator config
  
  /* location for generated Scala JNI interface objects */
  val scalaJNIDir = new File(Config.buildDir + "/scala/")
  
  /* location for generated native method wrappers */
  val nativeDir = new File(Config.buildDir + "/native/")
  
  /* location for compiled .so shared libraries */
  val libDir = new File(Config.buildDir + "/libraries/")
  
  override def initializeGenerator(buildDir: String) {
    scalaJNIDir.mkdirs()
    nativeDir.mkdirs()
    libDir.mkdirs()    
    super.initializeGenerator(buildDir)        
  }
  
  override def finalizeGenerator() {
    interfaceStreams foreach { v => val s = v._2; s.println("}"); s.close() }
    nativeStreams foreach { v => val s = v._2; s.close() }
    
    // compile native code into .so
    libraries foreach { v => val lib = v._1; lib.compile(new File(nativeDir, "/" + lib.JNIName + "." + lib.ext) toString, libDir.toString) }
    
    super.finalizeGenerator()    
  }
  
  //////////////////
  // generator state
  
  val libraries = HashMap[ExternalLibrary, Set[String]]() // from library to a set of method names
  val interfaceStreams = HashMap[ExternalLibrary, PrintWriter]() // from library to JNI interface printstreams
  val nativeStreams = HashMap[ExternalLibrary, PrintWriter]() // from library to native printstreams
    
  
  /////////////////
  // implementation
    
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = rhs match {
    case ThinDef(e:DeliteOpExternal[_]) =>
      if (!libraries.contains(e.lib)){
        emitJNIHeader(e.lib)
      }
      
      val methodSet = libraries.getOrElse(e.lib, Set[String]())
      if (!methodSet.contains(e.scalaFuncName)){
        emitJNIMethod(e.lib, e.scalaFuncSignature)
        emitJNINativeWrapper(e.lib, e.nativeFunc)        
      }
            
      libraries.put(e.lib, methodSet + e.scalaFuncName)
      super.emitFatNode(sym, rhs)
      
    case _ => super.emitFatNode(sym, rhs)
  }
  
  // one per library
  def emitJNIHeader(lib: ExternalLibrary) {
    assert(!interfaceStreams.contains(lib))
    assert(!nativeStreams.contains(lib))
    
    // interface file header
    val i = new PrintWriter(new File(scalaJNIDir, "/" + lib.JNIName + ".scala"))
    i.println("""
package %s
object %s {
System.load("%s")
""".format("generated.scala",lib.JNIName,new File(libDir, "/" + lib.JNIName + ".so")))
    interfaceStreams.put(lib, i)
    
    // native file header
    val n = new PrintWriter(new File(nativeDir, "/" + lib.JNIName + "." + lib.ext))
    n.println(lib.header)
    nativeStreams.put(lib, n)    
  }
  
  // one per method into shared file <libName>.scala
  def emitJNIMethod(lib: ExternalLibrary, method: String) {
    val s = interfaceStreams.getOrElse(lib, throw new IllegalArgumentException("tried to emit JNI header without an open interface file"))
    s.println("@native")
    s.println(method)
    s.println("")
  }
  
  // one per method into shared file <libName>.c
  def emitJNINativeWrapper(lib: ExternalLibrary, nativeFunc: String) {
    val s = nativeStreams.getOrElse(lib, throw new IllegalArgumentException("tried to emit JNI native wrapper without an open native file"))
    s.println(nativeFunc)
    s.println("")    
  }
}