package ppl.delite.framework.ops

import scala.virtualization.lms.internal.GenerationFailedException
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait DeliteFileStream

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
}

trait DeliteFileReaderOpsExp extends DeliteFileReaderOps with DeliteArrayOpsExpOpt with DeliteArrayBufferOpsExp with DeliteOpsExp with DeliteMapOpsExp {

  //Note: DeliteFileStream cannot currently be used outside of a MultiLoop because openAtNewLine() is not exposed
  case class DeliteFileStreamNew(paths: Seq[Exp[String]], charset: Rep[String], delimiter: Rep[DeliteArray[Byte]]) extends Def[DeliteFileStream]
  def dfs_new(paths: Seq[Exp[String]], charset: Rep[String] = unit(null), delimiter: Rep[DeliteArray[Byte]] = unit(null))(implicit pos: SourceContext) = reflectPure(DeliteFileStreamNew(paths, charset, delimiter))

  case class DeliteFileStreamReadLine(stream: Exp[DeliteFileStream], idx: Exp[Int]) extends Def[String]
  def dfs_readLine(stream: Exp[DeliteFileStream], idx: Exp[Int])(implicit pos: SourceContext): Exp[String] = reflectPure(DeliteFileStreamReadLine(stream, idx))

  case class DeliteFileStreamReadBytes(stream: Exp[DeliteFileStream], idx: Exp[Int]) extends Def[DeliteArray[Byte]]
  def dfs_readBytes(stream: Exp[DeliteFileStream], idx: Exp[Int])(implicit pos: SourceContext): Exp[DeliteArray[Byte]] = reflectPure(DeliteFileStreamReadBytes(stream, idx))

  case class DeliteFileStreamSize(stream: Exp[DeliteFileStream]) extends Def[Long]
  def dfs_size(stream: Exp[DeliteFileStream])(implicit pos: SourceContext): Exp[Int] = reflectPure(DeliteFileStreamSize(stream)).asInstanceOf[Exp[Int]] //sketchy...

  def dfr_readLines[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[A])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderLines(paths, charset, f))
  def dfr_readLinesFlattened[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderFlatLines(paths, charset, f))
  def dfr_readBytes[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[A])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderBytes(paths, delimiter, f))
  def dfr_readBytesFlattened[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderFlatBytes(paths, delimiter, f))

  case class DeliteOpFileReaderLines[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[A])(implicit pos: SourceContext) extends DeliteOpFileReaderI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfs_new(paths, charset = charset)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfs_readLine(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  case class DeliteOpFileReaderBytes[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[A])(implicit pos: SourceContext) extends DeliteOpFileReaderI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfs_new(paths, delimiter = delimiter)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfs_readBytes(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  abstract class DeliteOpFileReaderI[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest]
    extends DeliteOpMapLike[A,I,CA] {
    type OpType <: DeliteOpFileReaderI[A,I,CA]

    def func: Exp[Int] => Exp[A]

    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](
      func = reifyEffects(func(v)),
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmI = manifest[I]
    val dmCA = manifest[CA]
  }

  case class DeliteOpFileReaderFlatLines[A:Manifest](paths: Seq[Exp[String]], charset: Exp[String], f: Exp[String] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) extends DeliteOpFileReaderFlatI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfs_new(paths, charset = charset)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfs_readLine(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  case class DeliteOpFileReaderFlatBytes[A:Manifest](paths: Seq[Exp[String]], delimiter: Exp[DeliteArray[Byte]], f: Exp[DeliteArray[Byte]] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) extends DeliteOpFileReaderFlatI[A,DeliteArray[A],DeliteArray[A]] {
    val inputStream = dfs_new(paths, delimiter = delimiter)
    val size = copyTransformedOrElse(_.size)(dfs_size(inputStream))
    def func = idx => f(dfs_readBytes(inputStream,idx))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  abstract class DeliteOpFileReaderFlatI[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest]
    extends DeliteOpMapLike[A,I,CA] {
    type OpType <: DeliteOpFileReaderFlatI[A,I,CA]

    def func: Exp[Int] => Exp[DeliteCollection[A]]

    final lazy val iFunc: Exp[DeliteCollection[A]] = copyTransformedOrElse(_.iFunc)(func(v))
    final lazy val iF: Sym[Int] = copyTransformedOrElse(_.iF)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eF: Sym[DeliteCollection[A]] = copyTransformedOrElse(_.eF)(fresh[DeliteCollection[A]](iFunc.tp)).asInstanceOf[Sym[DeliteCollection[A]]]

    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](
      iFunc = Some(reifyEffects(this.iFunc)),
      iF = Some(this.iF),
      sF = Some(reifyEffects(dc_size(eF))), //note: applying dc_size directly to iFunc can lead to iFunc being duplicated (during mirroring?)
      eF = Some(this.eF),
      func = reifyEffects(dc_apply(eF,iF)),
      par = dc_parallelization(allocVal, true),
      buf = this.buf,
      numDynamicChunks = this.numDynamicChunks
    ))

    val dmA = manifest[A]
    val dmI = manifest[I]
    val dmCA = manifest[CA]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteOpFileReaderLines(paths,ch,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderLines(f(paths),f(ch),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderLines(paths,ch,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderLines(f(paths),f(ch),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteOpFileReaderBytes(paths,delim,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderBytes(f(paths),f(delim),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderBytes(paths,delim,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderBytes(f(paths),f(delim),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteOpFileReaderFlatLines(paths,ch,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatLines(f(paths),f(ch),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderFlatLines(paths,ch,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatLines(f(paths),f(ch),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteOpFileReaderFlatBytes(paths,delim,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatBytes(f(paths),f(delim),f(func))(e.dmA,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderFlatBytes(paths,delim,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderFlatBytes(f(paths),f(delim),f(func))(e.dmA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileStreamNew(paths,ch,delim) => dfs_new(f(paths),f(ch),f(delim))
    case Reflect(DeliteFileStreamNew(paths,ch,delim), u, es) => reflectMirrored(Reflect(DeliteFileStreamNew(f(paths),f(ch),f(delim)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileStreamReadLine(stream,idx) => dfs_readLine(f(stream),f(idx))
    case Reflect(DeliteFileStreamReadLine(stream,idx), u, es) => reflectMirrored(Reflect(DeliteFileStreamReadLine(f(stream), f(idx)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileStreamReadBytes(stream,idx) => dfs_readBytes(f(stream),f(idx))
    case Reflect(DeliteFileStreamReadBytes(stream,idx), u, es) => reflectMirrored(Reflect(DeliteFileStreamReadBytes(f(stream), f(idx)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case DeliteFileStreamSize(stream) => dfs_size(f(stream))
    case Reflect(DeliteFileStreamSize(stream), u, es) => reflectMirrored(Reflect(DeliteFileStreamSize(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenDeliteFileReaderOps extends ScalaGenFat {
  val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteFileStreamNew(paths, charset, Const(null)) =>
      emitValDef(sym, "generated.scala.io.FileStreamImpl(" + quote(charset) + "," + paths.map(quote).mkString("Seq(",",",")") + ")")
    case DeliteFileStreamNew(paths, Const(null), delimiter) =>
      emitValDef(sym, "generated.scala.io.FileStreamImpl.bytes(" + quote(delimiter) + "," + paths.map(quote).mkString("Seq(",",",")") + ")")
    case DeliteFileStreamReadLine(stream,idx) =>
      emitValDef(sym, quote(stream) + "_stream.readLine()")
    case DeliteFileStreamReadBytes(stream,idx) =>
      emitValDef(sym, quote(stream) + "_stream.readBytes()")
    case DeliteFileStreamSize(stream) =>
      emitValDef(sym, quote(stream) + ".size")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteFileStream" => "generated.scala.io.FileStreamImpl"
    case _ => super.remap(m)
  }

}

trait CGenDeliteFileReaderOps extends CGenFat {
  val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteFileStreamNew(paths, Const(null), Const(null)) => 
      // C++ variable length args does not allow string types, so use underlying char *
      if (cppMemMgr == "refcnt")
        stream.println(remap(sym.tp) + " " + quote(sym) + "(new cppFileStream(" + paths.length + "," + paths.map(quote(_) + ".c_str()").mkString(",") + "));")
      else
        emitValDef(sym, "new cppFileStream(" + paths.length + "," + paths.map(quote(_) + ".c_str()").mkString(",") + ")")
    case DeliteFileStreamNew(paths, charset, delimiter) => 
      throw new GenerationFailedException("FileReader: custom charset/delimiter is not suppported by C codegen")
    case DeliteFileStreamReadLine(stream,idx) =>
      emitValDef(sym, quote(stream) + "_stream->readLine("+resourceInfoSym+")")
    case DeliteFileStreamSize(stream) =>
      emitValDef(sym, quote(stream) + "->size")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteFileStream" if (cppMemMgr == "refcnt") => wrapSharedPtr("cppFileStream")
    case "DeliteFileStream" => "cppFileStream"
    case _ => super.remap(m)
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"cppFileStream.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }

}
