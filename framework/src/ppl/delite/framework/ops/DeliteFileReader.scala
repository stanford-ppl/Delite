package ppl.delite.framework.ops

import scala.virtualization.lms.internal.GenerationFailedException
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait DeliteFileInputStream

trait DeliteFileReaderOps extends Base with DeliteArrayBufferOps {

  object DeliteFileReader {
    def readLines[A:Manifest](paths: Rep[String]*)(f: Rep[String] => Rep[A])(implicit pos: SourceContext) = dfr_readLines(paths, unit(null), f)
    def readLinesFlattened[A:Manifest](paths: Rep[String]*)(f: Rep[String] => Rep[DeliteCollection[A]])(implicit pos: SourceContext) = dfr_readLinesFlattened(paths, unit(null), f)
    //TODO: is there any way of overloading readLines with these types?
    def readLinesC[A:Manifest](charset: Rep[String], paths: Rep[String]*)(f: Rep[String] => Rep[A])(implicit pos: SourceContext) = dfr_readLines(paths, charset, f)
    def readLinesFlattenedC[A:Manifest](charset: Rep[String], paths: Rep[String]*)(f: Rep[String] => Rep[DeliteCollection[A]])(implicit pos: SourceContext) = dfr_readLinesFlattened(paths, charset, f)
    def readBytes[A:Manifest](delimiter: Rep[DeliteArray[Byte]], paths: Rep[String]*)(f: Rep[DeliteArray[Byte]] => Rep[A])(implicit pos: SourceContext) = dfr_readBytes(paths, delimiter, f)
    def readBytesFlattened[A:Manifest](delimiter: Rep[DeliteArray[Byte]], paths: Rep[String]*)(f: Rep[DeliteArray[Byte]] => Rep[DeliteCollection[A]])(implicit pos: SourceContext) = dfr_readBytesFlattened(paths, delimiter, f)
  }
  def dfr_readLines[A:Manifest](paths: Seq[Rep[String]], charset: Rep[String], f: Rep[String] => Rep[A])(implicit pos: SourceContext): Rep[DeliteArray[A]]
  def dfr_readLinesFlattened[A:Manifest](paths: Seq[Rep[String]], charset: Rep[String], f: Rep[String] => Rep[DeliteCollection[A]])(implicit pos: SourceContext): Rep[DeliteArray[A]]
  def dfr_readBytes[A:Manifest](paths: Seq[Rep[String]], delimiter: Rep[DeliteArray[Byte]], f: Rep[DeliteArray[Byte]] => Rep[A])(implicit pos: SourceContext): Rep[DeliteArray[A]]
  def dfr_readBytesFlattened[A:Manifest](paths: Seq[Rep[String]], delimiter: Rep[DeliteArray[Byte]], f: Rep[DeliteArray[Byte]] => Rep[DeliteCollection[A]])(implicit pos: SourceContext): Rep[DeliteArray[A]]

  // These methods are used to initialize and read DeliteFileInputStreams from within multiloops (no effects)
  def dfis_new(paths: Seq[Rep[String]], charset: Rep[String] = unit(null), delimiter: Rep[DeliteArray[Byte]] = unit(null))(implicit pos: SourceContext): Rep[DeliteFileInputStream]
  def dfis_readLine(stream: Rep[DeliteFileInputStream], idx: Rep[Int])(implicit pos: SourceContext): Rep[String]

  // These methods are effectful and meant for use outside of parallel loops
  def dfis_new_effectful(paths: Seq[Rep[String]], charset: Rep[String] = unit(null), delimiter: Rep[DeliteArray[Byte]] = unit(null))(implicit pos: SourceContext): Rep[DeliteFileInputStream]
  def dfis_readLine_effectful(stream: Rep[DeliteFileInputStream])(implicit pos: SourceContext): Rep[String]
  def dfis_close(stream: Rep[DeliteFileInputStream])(implicit pos: SourceContext): Rep[Unit]
}

trait DeliteFileReaderOpsExp extends DeliteFileReaderOps with DeliteArrayOpsExpOpt with DeliteArrayBufferOpsExp with DeliteOpsExp with DeliteMapOpsExp {

  case class DeliteFileInputStreamNew(paths: Seq[Exp[String]], charset: Rep[String], delimiter: Rep[DeliteArray[Byte]]) extends Def[DeliteFileInputStream]
  def dfis_new(paths: Seq[Exp[String]], charset: Rep[String] = unit(null), delimiter: Rep[DeliteArray[Byte]] = unit(null))(implicit pos: SourceContext) = reflectPure(DeliteFileInputStreamNew(paths, charset, delimiter))
  def dfis_new_effectful(paths: Seq[Exp[String]], charset: Rep[String] = unit(null), delimiter: Rep[DeliteArray[Byte]] = unit(null))(implicit pos: SourceContext) = reflectEffect(DeliteFileInputStreamNew(paths, charset, delimiter))

  // This version of ReadLine can only be used inside multi-loops. It relies on the unexposed openAtNewLine() function,
  // which is called inside DeliteOps codegen to initialize the stream for each thread.
  case class DeliteFileInputStreamReadLine(stream: Exp[DeliteFileInputStream], idx: Exp[Int]) extends Def[String]
  def dfis_readLine(stream: Exp[DeliteFileInputStream], idx: Exp[Int])(implicit pos: SourceContext): Exp[String] = reflectPure(DeliteFileInputStreamReadLine(stream, idx))

  // This version of ReadLine can only be used outside of multi-loops (sequentially)
  case class DeliteFileInputStreamReadLineEffectful(stream: Exp[DeliteFileInputStream]) extends Def[String]
  def dfis_readLine_effectful(stream: Exp[DeliteFileInputStream])(implicit pos: SourceContext): Exp[String] = reflectEffect(DeliteFileInputStreamReadLineEffectful(stream))

  case class DeliteFileInputStreamReadBytes(stream: Exp[DeliteFileInputStream], idx: Exp[Int]) extends Def[DeliteArray[Byte]]
  def dfs_readBytes(stream: Exp[DeliteFileInputStream], idx: Exp[Int])(implicit pos: SourceContext): Exp[DeliteArray[Byte]] = reflectPure(DeliteFileInputStreamReadBytes(stream, idx))

  case class DeliteFileInputStreamSize(stream: Exp[DeliteFileInputStream]) extends Def[Long]
  def dfs_size(stream: Exp[DeliteFileInputStream])(implicit pos: SourceContext): Exp[Int] = reflectPure(DeliteFileInputStreamSize(stream)).asInstanceOf[Exp[Int]] //sketchy...

  def dfr_readLines[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[A])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderLines(paths, charset, f))
  def dfr_readLinesFlattened[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderFlatLines(paths, charset, f))
  def dfr_readBytes[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[A])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderBytes(paths, delimiter, f))
  def dfr_readBytesFlattened[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderFlatBytes(paths, delimiter, f))

  case class DeliteOpFileReaderLines[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[A])(implicit pos: SourceContext) extends DeliteOpFileReaderI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfis_new(paths, charset = charset)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfis_readLine(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  case class DeliteOpFileReaderBytes[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[A])(implicit pos: SourceContext) extends DeliteOpFileReaderI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfis_new(paths, delimiter = delimiter)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfs_readBytes(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  abstract class DeliteOpFileReaderI[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest]
      extends DeliteOpMapLike[A,I,CA] {
    type OpType <: DeliteOpFileReaderI[A,I,CA]

    def func: Exp[Int] => Exp[A]

    override def mapFunc(): Exp[A] = func(v)
    // despite being logically a map, the output size will only be known at runtime
    override val unknownOutputSize = true
    val dmA = manifest[A]
    val dmCA = manifest[CA]
  }

  case class DeliteOpFileReaderFlatLines[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) extends DeliteOpFileReaderFlatI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfis_new(paths, charset = charset)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfis_readLine(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  case class DeliteOpFileReaderFlatBytes[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) extends DeliteOpFileReaderFlatI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfis_new(paths, delimiter = delimiter)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfs_readBytes(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  abstract class DeliteOpFileReaderFlatI[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest]
    extends DeliteOpFlatMapLike[A,I,CA] {
    type OpType <: DeliteOpFileReaderFlatI[A,I,CA]

    def func: Exp[Int] => Exp[DeliteCollection[A]]
    
    override def flatMapLikeFunc(): Exp[DeliteCollection[A]] = func(v)

    val dmA = manifest[A]
    val dmCA = manifest[CA]
  }

  case class DeliteFileInputStreamClose(stream: Exp[DeliteFileInputStream]) extends Def[Unit]

  // not explicitly called from DeliteFileReader.readLines/readBytes, since the effect can prevent fusion of the parallel loops
  def dfis_close(stream: Rep[DeliteFileInputStream])(implicit pos: SourceContext): Rep[Unit] = reflectEffect(DeliteFileInputStreamClose(stream))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteOpFileReaderLines(paths,ch,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderLines(f(paths),f(ch),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderLines(paths,ch,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderLines(f(paths),f(ch),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteOpFileReaderBytes(paths,delim,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderBytes(f(paths),f(delim),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderBytes(paths,delim,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderBytes(f(paths),f(delim),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteOpFileReaderFlatLines(paths,ch,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatLines(f(paths),f(ch),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderFlatLines(paths,ch,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatLines(f(paths),f(ch),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteOpFileReaderFlatBytes(paths,delim,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatBytes(f(paths),f(delim),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderFlatBytes(paths,delim,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatBytes(f(paths),f(delim),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileInputStreamNew(paths,ch,delim) => dfis_new(f(paths),f(ch),f(delim))
    case Reflect(DeliteFileInputStreamNew(paths,ch,delim), u, es) => reflectMirrored(Reflect(DeliteFileInputStreamNew(f(paths),f(ch),f(delim)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileInputStreamReadLine(stream,idx) => dfis_readLine(f(stream),f(idx))
    case Reflect(DeliteFileInputStreamReadLine(stream,idx), u, es) => reflectMirrored(Reflect(DeliteFileInputStreamReadLine(f(stream), f(idx)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(DeliteFileInputStreamReadLineEffectful(stream), u, es) => reflectMirrored(Reflect(DeliteFileInputStreamReadLineEffectful(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileInputStreamReadBytes(stream,idx) => dfs_readBytes(f(stream),f(idx))
    case Reflect(DeliteFileInputStreamReadBytes(stream,idx), u, es) => reflectMirrored(Reflect(DeliteFileInputStreamReadBytes(f(stream), f(idx)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileInputStreamSize(stream) => dfs_size(f(stream))
    case Reflect(DeliteFileInputStreamSize(stream), u, es) => reflectMirrored(Reflect(DeliteFileInputStreamSize(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(DeliteFileInputStreamClose(stream), u, es) => reflectMirrored(Reflect(DeliteFileInputStreamClose(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenDeliteFileReaderOps extends ScalaGenFat {
  val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteFileInputStreamNew(paths, Const(null), Const(null)) =>
      emitValDef(sym, "generated.scala.io.DeliteFileInputStream("+paths.map(quote).mkString("Seq(",",",")") + ", None, None)")
    case DeliteFileInputStreamNew(paths, charset, Const(null)) =>
      emitValDef(sym, "generated.scala.io.DeliteFileInputStream("+paths.map(quote).mkString("Seq(",",",")") + ", Some(" + quote(charset) + "), None)")
    case DeliteFileInputStreamNew(paths, Const(null), delimiter) =>
      emitValDef(sym, "generated.scala.io.DeliteFileInputStream("+paths.map(quote).mkString("Seq(",",",")") + ", None, Some("+quote(delimiter) + "))")
    case DeliteFileInputStreamReadLine(stream,idx) =>
      emitValDef(sym, quote(stream) + "_stream.readLine()")
    case DeliteFileInputStreamReadLineEffectful(stream) =>
      emitValDef(sym, quote(stream) + ".readLine()")
    case DeliteFileInputStreamReadBytes(stream,idx) =>
      emitValDef(sym, quote(stream) + "_stream.readBytes()")
    case DeliteFileInputStreamSize(stream) =>
      emitValDef(sym, quote(stream) + ".size")
    case DeliteFileInputStreamClose(stream) =>
      emitValDef(sym, quote(stream) + ".close()")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteFileInputStream" => "generated.scala.io.DeliteFileInputStream"
    case _ => super.remap(m)
  }

}

trait CGenDeliteFileReaderOps extends CGenFat {
  val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteFileInputStreamNew(paths, Const(null), Const(null)) =>
      // C++ variable length args does not allow string types, so use underlying char *
      if (cppMemMgr == "refcnt")
        stream.println(remap(sym.tp) + " " + quote(sym) + "(new cppFileStream(" + paths.length + "," + paths.map(quote(_) + ".c_str()").mkString(",") + "));")
      else
        emitValDef(sym, "new cppFileStream(" + paths.length + "," + paths.map(quote(_) + ".c_str()").mkString(",") + ")")
    case DeliteFileInputStreamNew(paths, charset, delimiter) =>
      throw new GenerationFailedException("FileReader: custom charset/delimiter is not suppported by C codegen")
    case DeliteFileInputStreamReadLine(stream,idx) =>
      emitValDef(sym, quote(stream) + "_stream->readLine("+resourceInfoSym+")")
    case DeliteFileInputStreamSize(stream) =>
      emitValDef(sym, quote(stream) + "->size")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteFileInputStream" if (cppMemMgr == "refcnt") => wrapSharedPtr("cppFileStream")
    case "DeliteFileInputStream" => "cppFileStream"
    case _ => super.remap(m)
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"cppFileStream.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }

}
