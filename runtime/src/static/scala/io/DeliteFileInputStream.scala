package generated.scala.io

import com.google.protobuf.ByteString
import java.io.{InputStream, ByteArrayInputStream, IOException, DataInput, DataOutput}
import java.nio.charset.Charset

import org.apache.hadoop.conf._
import org.apache.hadoop.fs._
import org.apache.hadoop.io.compress.GzipCodec
import org.apache.hadoop.io.Text

/**
 * Constructs a logical file stream by concatenating the streams of an arbitrary number of physical files.
 * The stream can be opened at any byte offset and it will automatically seek to the next full line.
 * Each path in the stream can be backed by a different Hadoop-supported filesystem (inclues local, HDFS and S3).
 */

object DeliteFileInputStream {

  // We sometimes have an issue with checksums when appending to DeliteFileOutputStreams using a
  // RawLocalFileSystem (see comment in DeliteFileOutputStream). Using RawLocalFileSystem should
  // bypass checksums, but the checksum file still seems to get created and checked when read again.
  // For now, simply disabling checksums by default as a workaround.
  val CHECKSUM_ENABLED = System.getProperty("dfis.checksum.enabled", "false").toBoolean

  /* Construct a new DeliteFileInputStream */
  def apply(paths: Seq[String], charsetName: Option[String] = None, delimiter: Option[Array[Byte]] = None, offset: Long = 0L): DeliteFileInputStream = {
    val charset = checkCharset(charsetName)
    val conf = new Configuration()
    // We pre-load the file handles so that we can easily copy the stream wrapper instance at run-time
    val fileHandles = getFiles(conf, paths)
    new DeliteFileInputStream(conf, fileHandles, charset, delimiter, offset)
  }

  /* Each path must refer to a valid filesystem, based on the Hadoop configuration object. */
  private def getFiles(conf: Configuration, paths:Seq[String]) = paths.toArray flatMap { p =>
    val hPath = new Path(p)
    val fs = hPath.getFileSystem(conf) //works on globbed paths as long as the prefix is constant
    val expanded = fs.globStatus(hPath)

    if (expanded == null) throw new IOException("Path " + hPath + " does not appear to be a valid file or directory")

    // recurse into sub-directories
    def listStatus(s: FileStatus): Array[FileStatus] = {
      fs.listStatus(s.getPath) flatMap { s =>
        if (s.isDirectory) listStatus(s)
        else Array(s)
      }
    }

    expanded flatMap { status =>
      if (status.isDirectory) {
        // return sorted list of file statuses (numbered file chunks should be parsed in order)
        listStatus(status).sortBy(f => f.getPath.getName)
      }
      else if (status.isFile) Array(status)
      else throw new IOException("Path " + status.getPath + " does not appear to be a valid file or directory")
    }
  }

  /* Validate that the specified charset name is legal and supported */
  private def checkCharset(charsetName: Option[String]) = {
    val charset = charsetName map { Charset.forName } getOrElse Charset.defaultCharset

    //we only support backwards-compatible charsets (all extended ascii and utf-8); this is a hacky way of checking that
    val dec = charset.newDecoder
    if (dec.maxCharsPerByte != 1f || dec.averageCharsPerByte != 1f)
      throw new IOException("Unsupported Charset: " + charset.displayName)
    charset
  }

  /* Deserialization for a DeliteFileInputStream, using Hadoop serialization for Hadoop objects */
  def deserialize(bytes: ByteString): DeliteFileInputStream = {
    val in = new java.io.DataInputStream(bytes.newInput)
    val conf = new Configuration()
    conf.readFields(in)
    val filesLen = in.readInt()
    val files = Array.fill(filesLen) {
      val fs = new FileStatus()
      fs.readFields(in)
      fs
    }
    val charset = Charset.defaultCharset
    val delimiterExists = in.readBoolean()
    val delimiter =
      if (delimiterExists) {
        val delimiterLen = in.readInt()
        Some(Array.fill(delimiterLen) { in.readByte() })
      }
      else None

    val streamOffset = in.readLong()
    val dfis = new DeliteFileInputStream(conf, files, charset, delimiter, streamOffset)
    val pos = in.readLong()
    dfis.openAtNewLine(pos)
    dfis
  }

}

class DeliteFileInputStream(conf: Configuration, files: Array[FileStatus], charset: Charset, delimiter: Option[Array[Byte]], val streamOffset: Long) {
  private[this] var reader: LineReader = _
  private[this] var text: Text = _

  private[this] var pos: Long = _ //relative position within current physical file
  private[this] var fileIdx: Int = _ //current index into 'files' array
  private[this] var fileName: String = _
  private[this] var fileIsCompressed: Boolean = _

  private[this] var endIdx: Int = _
  private[this] var endPos: Long = _
  private[this] var endOfStream: Boolean = false

  private[this] val codec = new GzipCodec //TODO: support multiple codecs
  codec.setConf(conf)

  final val size: Long = files.map(_.getLen).sum

  private def setState(_pos: Long, _fileIdx: Int, _fileName: String) = {
    pos = _pos; fileIdx = _fileIdx; fileName = _fileName
    fileIsCompressed = fileName.endsWith(codec.getDefaultExtension)
  }

  /* Determine the file that this logical index corresponds to, as well as the byte offset within the file. */
  private def findFileOffsets(start: Long, end: Long) = {
    //find the last equivalent local position for 'start'
    var startOffset = start
    var startIdx = 0
    while (startIdx < files.length && startOffset >= files(startIdx).getLen) {
      startOffset -= files(startIdx).getLen
      startIdx += 1
    }

    //find the first equivalent local position for 'end' (subtly different comparator)
    var endOffset = end
    var endIdx = 0
    while (endIdx < files.length && endOffset > files(endIdx).getLen) {
      endOffset -= files(endIdx).getLen
      endIdx += 1
    }

    ((startIdx, startOffset), (endIdx, endOffset))
  }

  /*
   * Return an input stream opened at the next full line after 'offset'
   * Offset refers to the number of bytes inside the physical resource at 'path'
   */
  private def openInputStream(path: Path, offset: Long) = {
    val fs = path.getFileSystem(conf)
    fs.setVerifyChecksum(DeliteFileInputStream.CHECKSUM_ENABLED)

    val rawStream = fs.open(path)
    val byteStream = if (fileIsCompressed) codec.createInputStream(rawStream) else rawStream
    val reader = new LineReader(byteStream, delimiter.getOrElse(null))

    if (offset != 0) {
      byteStream.seek(offset-1) //seek to offset; -1 ensures we won't skip a full line on first readLine()
      pos += (reader.readLine(text) - 1) //jump to next full line
    }
    reader
  }

  private def openFileStream(startIdx: Int, offset: Long) = {
    if (startIdx >= files.length) throw new IndexOutOfBoundsException("Cannot open file at index: " + startIdx + ", num files is: " + files.length)
    val path = files(startIdx).getPath
    setState(offset, startIdx, path.getName)

    if (fileIsCompressed && offset != 0) {
      if (fileIdx < files.length-1) { //skip to beginning of next file
        val path = files(fileIdx+1).getPath
        setState(0, fileIdx+1, path.getName)
        openInputStream(path, 0)
      } else { //return empty stream
        setState(endPos, endIdx, path.getName)
        new LineReader(new ByteArrayInputStream(new Array[Byte](0)), delimiter.getOrElse(null))
      }
    } else {
      openInputStream(path, offset)
    }
  }

  /* Set the line reader to a newline-aligned input stream corresponding to logical byte index 'startPosition' */
  final def openAtNewLine(startPosition: Long, endPosition: Long = size) = {
    close()

    val start = streamOffset + startPosition
    val end = math.min(streamOffset + endPosition, size)
    if (start >= size) throw new IllegalArgumentException("Cannot load stream at position " + start + ", stream size is: " + size)
    if (end < start) throw new IllegalArgumentException("End of stream ("+end+") cannot be less than start of stream ("+start+")")

    val ((startIndex, startOffset), (endIndex, endOffset)) = findFileOffsets(start, end)
    endIdx = endIndex; endPos = endOffset
    text = new Text
    reader = openFileStream(startIndex, startOffset)
    checkEndOfStream()
    this
  }

  /* Construct a copy of this DeliteFileInputStream, but set to a different offset. */
  final def withOffset(offset: Long) = {
    new DeliteFileInputStream(conf, files, charset, delimiter, offset)
  }

  /* Construct a copy of this DeliteFileInputStream, starting at logical byte index 'start' */
  final def openCopyAtNewLine(start: Long, end: Long = size): DeliteFileInputStream = {
    val copy = new DeliteFileInputStream(conf, files, charset, delimiter, streamOffset)
    copy.openAtNewLine(start, end)
  }

  final def open() {
    if (size > 0) {
      openAtNewLine(0)
    }
    else {
      throw new IOException("DeliteFileInputStream opened with size == 0. Paths were: " + files.map(_.getPath.getName).mkString("[",",","]"))
    }
  }

  /* Read the next line */
  private def readLineInternal() {
    var length = reader.readLine(text)
    while (length == 0 && fileIdx < files.length-1) {
      reader.close()
      reader = openFileStream(fileIdx+1, 0)
      length = reader.readLine(text)
    }
    pos += length
    checkEndOfStream()
  }

  /* Read the next line and interpret it as a String */
  final def readLine(): String = {
    if (endOfStream) null
    else {
      readLineInternal()
      new String(text.getBytes, 0, text.getLength, charset)
    }
  }

  /* Read the next line and interpret it as bytes */
  final def readBytes(): Array[Byte] = {
    if (endOfStream) null
    else {
      readLineInternal()
      val record = new Array[Byte](text.getLength)
      System.arraycopy(text.getBytes, 0, record, 0, text.getLength)
      record
    }
  }

  private def checkEndOfStream() = {
    if (fileIdx >= files.length-1 && reader.isEmpty) { //out of files to read
      endOfStream = true
    }
    else if (fileIsCompressed) { //done at end of file
      if (fileIdx >= endIdx && reader.isEmpty) endOfStream = true
    }
    else { //done when past endPos
      if (fileIdx >= endIdx && pos >= endPos) endOfStream = true
    }
  }

  final def isEmpty() = endOfStream

  /* Close the DeliteFileInputStream */
  final def close(): Unit = {
    if (reader != null) {
      reader.close()
      reader = null
    }
    text = null
    setState(0, 0, "")
    endIdx = 0; endPos = 0;
    endOfStream = false;
  }

  final def getFileLocation(): String = {
    fileName + ":" + pos
  }


  /////////////////////////////////////////////////////////////////////////////
  // Serialization
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Serialize the DeliteFileInputStream using Hadoop serialization for Hadoop objects.
   */
  def serialize: ByteString = {
    val outBytes = ByteString.newOutput
    val out = new java.io.DataOutputStream(outBytes)
    conf.write(out)
    out.writeInt(files.length)
    files.foreach(f => f.write(out))
    if (charset != Charset.defaultCharset) throw new UnsupportedOperationException("Don't know how to serialize custom charset " + charset)
    if (delimiter.isDefined) {
      out.writeBoolean(true)
      out.writeInt(delimiter.get.length)
      delimiter.get.foreach { b => out.writeByte(b) }
    }
    else {
      out.writeBoolean(false)
    }
    out.writeLong(streamOffset)
    out.writeLong(pos)
    outBytes.toByteString
  }

  /* Deserialization is static (see DeliteFileInputStream.deserialize) */

}
