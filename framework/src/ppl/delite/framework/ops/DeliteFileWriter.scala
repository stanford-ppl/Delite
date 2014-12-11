package ppl.delite.framework.ops

import scala.virtualization.lms.internal.GenerationFailedException
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait DeliteFileOutputStream

trait DeliteFileWriterOps extends Base with DeliteArrayBufferOps {

  object DeliteFileWriter {
    def writeLines(path: Rep[String], numLines: Rep[Int])(f: Rep[Int] => Rep[String])(implicit pos: SourceContext) = dfw_writeLines(path, numLines, f)
  }

  def dfw_writeLines(path: Rep[String], numLines: Rep[Int], f: Rep[Int] => Rep[String])(implicit pos: SourceContext): Rep[Unit]

  def dfos_new(path: Rep[String], numThreads: Rep[Int])(implicit pos: SourceContext): Rep[DeliteFileOutputStream]
  def dfos_writeLine(stream: Rep[DeliteFileOutputStream], line: Rep[String])(implicit pos: SourceContext): Rep[Unit]
  def dfos_close(stream: Rep[DeliteFileOutputStream])(implicit pos: SourceContext): Rep[Unit]
}

trait DeliteFileWriterOpsExp extends DeliteFileWriterOps with RuntimeServiceOpsExp with DeliteArrayOpsExpOpt with DeliteArrayBufferOpsExp with DeliteOpsExp with DeliteMapOpsExp {

  case class DeliteFileOutputStreamNew(path: Exp[String], numFiles: Exp[Int]) extends Def[DeliteFileOutputStream]
  
  case class DeliteOpFileWriteLines(stream: Exp[DeliteFileOutputStream], numLines: Exp[Int], f: Exp[Int] => Exp[String])(implicit pos: SourceContext) extends DeliteOpIndexedLoop {
    // dynamicChunks should default to 0, but we are explicit here, since static chunking is assumed by the implementation
    override val numDynamicChunks = 0

    val size = copyTransformedOrElse(_.size)(numLines)
    def func = idx => dfos_writeLine(stream,f(idx))
  }

  case class DeliteFileOutputStreamWriteLine(stream: Exp[DeliteFileOutputStream], line: Exp[String]) extends Def[Unit]

  case class DeliteFileOutputStreamClose(stream: Exp[DeliteFileOutputStream]) extends Def[Unit]

  def dfos_new(path: Exp[String], numThreads: Exp[Int])(implicit pos: SourceContext) = reflectMutable(DeliteFileOutputStreamNew(path, numThreads))

  def dfw_writeLines(path: Exp[String], numLines: Exp[Int], f: Exp[Int] => Exp[String])(implicit pos: SourceContext) = {
    // We allocate 1 file in the output stream per thread, and pass in threadId at runtime as the fileIdx
    val stream = dfos_new(path, DELITE_NUM_THREADS)
    reflectWrite(stream)(DeliteOpFileWriteLines(stream, numLines, f))
    dfos_close(stream)
  }

  def dfos_writeLine(stream: Exp[DeliteFileOutputStream], line: Exp[String])(implicit pos: SourceContext): Exp[Unit] = {
    reflectWrite(stream)(DeliteFileOutputStreamWriteLine(stream, line))
  }

  def dfos_close(stream: Exp[DeliteFileOutputStream])(implicit pos: SourceContext) = reflectWrite(stream)(DeliteFileOutputStreamClose(stream))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(e@DeliteOpFileWriteLines(path,numLines,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileWriteLines(f(path),f(numLines),f(func))(ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(DeliteFileOutputStreamNew(path,numFiles), u, es) => reflectMirrored(Reflect(DeliteFileOutputStreamNew(f(path), f(numFiles)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(DeliteFileOutputStreamWriteLine(stream,line), u, es) => reflectMirrored(Reflect(DeliteFileOutputStreamWriteLine(f(stream), f(line)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(DeliteFileOutputStreamClose(stream), u, es) => reflectMirrored(Reflect(DeliteFileOutputStreamClose(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenDeliteFileWriterOps extends ScalaGenFat with GenericGenDeliteOps with ScalaGenRuntimeServiceOps {
  val IR: DeliteFileWriterOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteFileOutputStreamNew(path, numFiles) =>
      emitValDef(sym, "generated.scala.io.DeliteFileOutputStream("+quote(path)+"," +quote(numFiles)+")")
    case DeliteFileOutputStreamWriteLine(stream, line) =>
      emitValDef(sym, quote(stream) + ".writeLine("+fieldAccess(resourceInfoSym,"threadId")+","+quote(line)+")")
    case DeliteFileOutputStreamClose(stream) =>
      emitValDef(sym, quote(stream) + ".close()")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteFileOutputStream" => "generated.scala.io.DeliteFileOutputStream"
    case _ => super.remap(m)
  }

}

trait CGenDeliteFileWriterOps extends CGenFat {
  val IR: DeliteFileWriterOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteFileOutputStreamNew(path, numFiles) =>
      throw new GenerationFailedException("FileWriter: not suppported by C codegen")
    case _ => super.emitNode(sym, rhs)
  }
}

