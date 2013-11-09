package ppl.delite.framework.ops

import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait DeliteFileStream

trait DeliteFileReaderOps extends Base with DeliteArrayBufferOps {
  object DeliteFileReader {
    def readLines[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[A]): Rep[DeliteArray[A]] = dfr_readLines(path, (line, buf) => buf += f(line))
    def readLinesUnstructured[A:Manifest](path: Rep[String])(f: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]): Rep[DeliteArray[A]] = dfr_readLines(path, f)
  }
  def dfr_readLines[A:Manifest](path: Rep[String], f: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]): Rep[DeliteArray[A]]
 

  object DeliteFileStream {
    def apply(paths: Seq[Rep[String]])(implicit pos: SourceContext) = dfs_new(paths)
  }
  def dfs_new(paths: Seq[Rep[String]])(implicit pos: SourceContext): Rep[DeliteFileStream]

  object DeliteNewFileReader {
    def readLines[A:Manifest](paths: Rep[String]*)(f: Rep[String] => Rep[A])(implicit pos: SourceContext) = dnfw_readLines(paths, f)
    def readLinesFlattened[A:Manifest](paths: Rep[String]*)(f: Rep[String] => Rep[DeliteCollection[A]])(implicit pos: SourceContext) = dnfw_readLinesFlattened(paths, f)
  }
  def dnfw_readLines[A:Manifest](paths: Seq[Rep[String]], f: Rep[String] => Rep[A])(implicit pos: SourceContext): Rep[DeliteArray[A]]
  def dnfw_readLinesFlattened[A:Manifest](paths: Seq[Rep[String]], f: Rep[String] => Rep[DeliteCollection[A]])(implicit pos: SourceContext): Rep[DeliteArray[A]]

}

trait DeliteFileReaderOpsExp extends DeliteFileReaderOps with DeliteArrayOpsExpOpt with DeliteArrayBufferOpsExp with DeliteOpsExp with DeliteMapOpsExp {

  def dfr_readLines[A:Manifest](path: Rep[String], f: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]) = reflectPure(DeliteOpFileReaderReadLines(reifyEffects(path), f))
  case class DeliteOpFileReaderReadLines[A:Manifest](path: Block[String], func: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]) extends DeliteOpInput[DeliteArray[A]] {
    type OpType <: DeliteOpFileReaderReadLines[A]
    val mA = manifest[A]

    val line: Sym[String] = copyTransformedOrElse(_.line)(fresh[String]).asInstanceOf[Sym[String]]
    val allocVal: Sym[DeliteArrayBuffer[A]] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[DeliteArrayBuffer[A]])).asInstanceOf[Sym[DeliteArrayBuffer[A]]]
    val alloc: Block[DeliteArrayBuffer[A]] = copyTransformedBlockOrElse(_.alloc)(reifyEffects(DeliteArrayBuffer[A]()))
    val append: Block[Unit] = copyTransformedBlockOrElse(_.append)(reifyEffects(func(line, allocVal)))
    val finalizer: Block[DeliteArray[A]] = copyTransformedBlockOrElse(_.finalizer)(reifyEffects{ darray_buffer_raw_data(allocVal).take(allocVal.length) })
  }

  override def blocks(e: Any): List[Block[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.blocks(i) ::: blocks(i.alloc) ::: blocks(i.append) ::: blocks(i.finalizer)
    case _ => super.blocks(e)
  }  
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.syms(i) ::: syms(i.alloc) ::: syms(i.append) ::: syms(i.finalizer)
    case _ => super.syms(e)
  }
    
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.readSyms(i) ::: readSyms(i.alloc) ::: readSyms(i.append) ::: readSyms(i.finalizer)
    case _ => super.readSyms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.boundSyms(i) ::: effectSyms(i.alloc) ::: effectSyms(i.append) ::: syms(i.line) ::: syms(i.allocVal) ::: effectSyms(i.finalizer)
    case _ => super.boundSyms(e)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.symsFreq(i) ::: freqNormal(i.alloc) ::: freqHot(i.append) ::: freqNormal(i.finalizer)
    case _ => super.symsFreq(e)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => Nil    
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => syms(i.alloc)
    case _ => super.copySyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteOpFileReaderReadLines(path,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderReadLines(f(path),f(func))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])      
    case Reflect(e@DeliteOpFileReaderReadLines(path,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderReadLines(f(path),f(func))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case e@DeliteOpFileReader(paths, func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReader(f(paths),f(func))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReader(paths,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReader(f(paths),f(func))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case e@DeliteOpFileReaderFlat(paths, func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderFlat(f(paths),f(func))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteOpFileReaderFlat(paths,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderFlat(f(paths),f(func))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case DeliteFileStreamNew(paths) => dfs_new(f(paths))
    case Reflect(DeliteFileStreamNew(paths), u, es) => reflectMirrored(Reflect(DeliteFileStreamNew(f(paths)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case DeliteFileStreamReadLine(stream,idx) => dfs_readLine(f(stream),f(idx))
    case Reflect(DeliteFileStreamReadLine(stream,idx), u, es) => reflectMirrored(Reflect(DeliteFileStreamReadLine(f(stream), f(idx)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case DeliteFileStreamSize(stream) => dfs_size(f(stream))
    case Reflect(DeliteFileStreamSize(stream), u, es) => reflectMirrored(Reflect(DeliteFileStreamSize(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case DeliteFileStreamNumThreads(stream) => dfs_numThreads(f(stream))
    case Reflect(DeliteFileStreamNumThreads(stream), u, es) => reflectMirrored(Reflect(DeliteFileStreamNumThreads(f(stream)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  //new IO
  case class DeliteFileStreamNew(paths: Seq[Exp[String]]) extends Def[DeliteFileStream]
  def dfs_new(paths: Seq[Exp[String]])(implicit pos: SourceContext) = reflectPure(DeliteFileStreamNew(paths))

  case class DeliteFileStreamReadLine(stream: Exp[DeliteFileStream], idx: Exp[Int]) extends Def[String]
  def dfs_readLine(stream: Exp[DeliteFileStream], idx: Exp[Int]): Exp[String] = reflectPure(DeliteFileStreamReadLine(stream, idx))

  case class DeliteFileStreamSize(stream: Exp[DeliteFileStream]) extends Def[Long]
  def dfs_size(stream: Exp[DeliteFileStream]): Exp[Long] = reflectPure(DeliteFileStreamSize(stream))

  case class DeliteFileStreamNumThreads(stream: Exp[DeliteFileStream]) extends Def[Int]
  def dfs_numThreads(stream: Exp[DeliteFileStream]) = reflectPure(DeliteFileStreamNumThreads(stream))

  def dnfw_readLines[A:Manifest](paths: Seq[Exp[String]], f: Exp[String] => Exp[A])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReader(paths, f))
  def dnfw_readLinesFlattened[A:Manifest](paths: Seq[Exp[String]], f: Exp[String] => Exp[DeliteCollection[A]])(implicit pos: SourceContext) = reflectPure(DeliteOpFileReaderFlat(paths, f))

  case class DeliteOpFileReader[A:Manifest](paths: Seq[Exp[String]], func: Exp[String] => Exp[A]) extends DeliteOpFileReaderI[A,DeliteArray[A],DeliteArray[A]] {    
    val inputStream = DeliteFileStream(paths)
    val size = copyTransformedOrElse(_.size)(dfs_numThreads(inputStream))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  abstract class DeliteOpFileReaderI[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest]
    extends DeliteOpMapLike[A,I,CA] {
    type OpType <: DeliteOpFileReaderI[A,I,CA]
  
    val inputStream: Exp[DeliteFileStream]
    def func: Exp[String] => Exp[A]

    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](
      func = reifyEffects(func(dfs_readLine(inputStream, v))),
      par = dc_parallelization(allocVal, true),
      buf = this.buf
    ))

    val dmA = manifest[A]
    val dmI = manifest[I]
    val dmCA = manifest[CA]
  }

  case class DeliteOpFileReaderFlat[A:Manifest](paths: Seq[Exp[String]], func: Exp[String] => Exp[DeliteCollection[A]]) extends DeliteOpFileReaderFlatI[A,DeliteArray[A],DeliteArray[A]] {    
    val inputStream = DeliteFileStream(paths)
    val size = copyTransformedOrElse(_.size)(dfs_numThreads(inputStream))
    def finalizer(x: Exp[DeliteArray[A]]) = x
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  abstract class DeliteOpFileReaderFlatI[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest]
    extends DeliteOpMapLike[A,I,CA] {
    type OpType <: DeliteOpFileReaderFlatI[A,I,CA]

    val inputStream: Exp[DeliteFileStream]
    def func: Exp[String] => Exp[DeliteCollection[A]]

    final lazy val iFunc: Exp[DeliteCollection[A]] = copyTransformedOrElse(_.iFunc)(func(dfs_readLine(inputStream,v)))
    final lazy val iF: Sym[Int] = copyTransformedOrElse(_.iF)(fresh[Int]).asInstanceOf[Sym[Int]]
    final lazy val eF: Sym[DeliteCollection[A]] = copyTransformedOrElse(_.eF)(fresh[DeliteCollection[A]](iFunc.tp)).asInstanceOf[Sym[DeliteCollection[A]]]
    
    lazy val body: Def[CA] = copyBodyOrElse(DeliteCollectElem[A,I,CA](
      iFunc = Some(reifyEffects(this.iFunc)),
      iF = Some(this.iF),
      sF = Some(reifyEffects(dc_size(iFunc))),
      eF = Some(this.eF),
      func = reifyEffects(dc_apply(eF,iF)),
      par = dc_parallelization(allocVal, true),
      buf = this.buf
    ))
    
    val dmA = manifest[A]
    val dmI = manifest[I]
    val dmCA = manifest[CA]
  }

}

trait ScalaGenDeliteFileReaderOps extends ScalaGenFat {
  val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DeliteOpFileReaderReadLines[_] => //TODO: how much of these file reader impl wrappers do we want in the runtime?
      emitBlock(op.path)
      emitValDef(sym, "{")
        stream.println("val file = new java.io.File(" + quote(getBlockResult(op.path)) + ")")
        stream.println("val (start, end) = ppl.delite.runtime.DeliteMesosExecutor.getBlockSize(file)")
        stream.println("val input = new java.io.BufferedReader(new java.io.FileReader(file))")
        stream.println("var pos = start")
        stream.println("if (pos != 0) {")
          stream.println("input.skip(pos-1)")
          stream.println("pos += input.readLine().length") //+1-1
        stream.println("}")

        emitBlock(op.alloc)
        emitValDef(op.allocVal, quote(getBlockResult(op.alloc)))

        stream.println("var " + quote(op.line) + " = input.readLine()")
        stream.println("while(pos < end && " + quote(op.line) + " != null) {")
          emitBlock(op.append)
          stream.println("pos += " + quote(op.line) + ".length + 1") //TODO: could be 1 or 2 characters extra
          stream.println(quote(op.line) + " = input.readLine()")
        stream.println("}")
        stream.println("input.close()")
        emitBlock(op.finalizer)
        stream.println("val act = new activation_" + quote(sym))
        stream.println("act." + quote(sym) + " = " + quote(getBlockResult(op.finalizer)))
        stream.println("act")
      stream.println("}")

    case DeliteFileStreamNew(paths) =>
      emitValDef(sym, "new generated.scala.io.FileStreamImpl(" + paths.map(quote).mkString(",") + ")")
    case DeliteFileStreamReadLine(stream,idx) =>
      emitValDef(sym, quote(stream) + ".readLine(" + quote(idx) + ")")
    case DeliteFileStreamSize(stream) =>
      emitValDef(sym, quote(stream) + ".size")
    case DeliteFileStreamNumThreads(stream) =>
      emitValDef(sym, quote(stream) + ".numThreads")
    case _ => super.emitNode(sym, rhs)
  }

  override def emitNodeKernelExtra(syms: List[Sym[Any]], rhs: Def[Any]): Unit = rhs match {
    case op: DeliteOpFileReaderReadLines[_] => //TODO: use activation record / closure style like MultiLoop
      val sym = syms(0)
      val actType = "activation_" + quote(sym)
      stream.println("final class " + actType + " {")
        stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = _")
        
        if (Config.generateSerializable) {
          stream.println("def combine(act: " + actType + ", rhs: " + actType + ") {")
          val tpe = remap(sym.tp)
          val obj = if (tpe.contains("DeliteArrayObject")) tpe.take(tpe.indexOf("[")) else tpe
          stream.println("act." + quote(sym) + " = " + obj + ".combine(act." + quote(sym) + "," + "rhs." + quote(sym) + ")")
          stream.println("}")

          stream.println("def serialize(): java.util.ArrayList[com.google.protobuf.ByteString] = {")
            stream.println("val arr = new java.util.ArrayList[com.google.protobuf.ByteString]")
            stream.println("arr.add(ppl.delite.runtime.messages.Serialization.serialize(this." + quote(sym) + ", true, \"" + quote(sym) + "\"))")
            stream.println("arr")
          stream.println("}")
        }
        stream.println("}")

        if (Config.generateSerializable) {
          stream.println("object " + actType + " {")
            stream.println("def deserialize(bytes: java.util.List[com.google.protobuf.ByteString]) = {")
              stream.println("val act = new " + actType)
              stream.println("act." + quote(sym) + " = ppl.delite.runtime.messages.Serialization.deserialize(classOf[" + remap(sym.tp) + "], bytes.get(0))")
              stream.println("act")
            stream.println("}")
          stream.println("}")
      }
    case _ => super.emitNodeKernelExtra(syms, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteFileStream" => "generated.scala.io.FileStreamImpl"
    case _ => super.remap(m)
  }

  /* def dfr_readLines_impl_cluster = {
    //Delite runtime magic to produce config... (& blocks?)
    val path = Path(filePath)
    val fs = FileSystem.get(config)
    val status = fs.getFileStatus(path)
    val blocks = fs.getFileBlockLocations(fileStatus, 0, status.getLen)
    for (block <- blocks if block.getHosts contains myHostName) {
      val start = block.getOffset
      val end = start + block.getLength
      //do remainder within loop ... concatenate blocks? are they contiguous?

    val file = fs.open(path) //in or out of loop? ...
    if (start != 0) {
      file.seek(start) //TODO: adjust for broken lines
    }
    val input = LineReader(file)
    val buffer = emitAlloc()
    var line = Text()
    var pos = start
    var lineSize = input.readLine(line)
    while (pos < end && lineSize > 0) {
      val lineSym = line.toString
      emitAppend()
      line = Text()
      pos += lineSize
      lineSize = input.readLine(line)
    }
    input.close()
    buffer
  } */

}
