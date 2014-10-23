package generated.scala.io

import com.google.protobuf.ByteString
import java.io.{InputStream, IOException}
import java.nio.charset.Charset

import org.apache.hadoop.conf._
import org.apache.hadoop.fs._
import org.apache.hadoop.io.Text
import org.apache.hadoop.util.LineReader

/**
 * Constructs a logical file stream by concatenating the streams of an arbitrary number of physical files.
 * The stream can be opened at any byte offset and it will automatically seek to the next full line.
 * Each path in the stream can be backed by a different Hadoop-supported filesystem (inclues local, HDFS and S3).
 *
 * Currently supports UTF-8 and extended-ASCII charsets; should be extensible to any multi-byte charset that
 * is self-synchronizing (e.g., UTF-16)). Also supports binary files via a user-provided custom record delimiter.
 */
object DeliteFileStream {

  /* Construct a new DeliteFileStream */
  def apply(paths: Seq[String], charsetName: Option[String] = None, delimiter: Option[Array[Byte]] = None): DeliteFileStream = {
    val charset = charsetName map { checkCharset }
    val conf = new Configuration()
    new DeliteFileStream(conf, getFiles(conf, paths), charset, delimiter)
  }

  /* Each path must refer to a valid filesystem, based on the Hadoop configuration object. */
  private def getFiles(conf: Configuration, paths:Seq[String]) = paths.toArray flatMap { p =>
    val hPath = new Path(p)
    val fs = hPath.getFileSystem(conf)
    val status = fs.getFileStatus(hPath)

    if (fs.isDirectory(hPath)) {
      // return sorted list of file statuses (numbered file chunks should be parsed in order)
      fs.listStatus(hPath).sortBy(f => f.getPath.getName)
    }
    else if (fs.isFile(hPath)) Array(fs.getFileStatus(hPath))
    else throw new IllegalArgumentException("Path " + p + " does not appear to be a valid file or directory")
  }

  /* Validate that the specified charset name is legal and supported */
  private def checkCharset(charsetName: String) = {
    val charset = if (charsetName eq null) Charset.defaultCharset else Charset.forName(charsetName)
    val dec = charset.newDecoder
    if (dec.maxCharsPerByte != 1f || dec.averageCharsPerByte != 1f)
      throw new IOException("Unsupported Charset: " + charset.displayName)
    charset
  }

}

class DeliteFileStream(conf: Configuration, files: Array[FileStatus], charset: Option[Charset], delimiter: Option[Array[Byte]]) {
  private[this] var reader: LineReader = _
  private[this] var text: Text = _
  private[this] var pos: Long = _

  final val size: Long = files.map(_.getLen).sum
  final def position = pos

  /* Determine the file that this logical index corresponds to, as well as the byte offset within the file. */
  private def findFileOffset(start: Long) = {
    var offset = start
    var fileIdx = 0
    while (offset >= files(fileIdx).getLen) {
      offset -= files(fileIdx).getLen
      fileIdx += 1
    }
    (fileIdx, offset)
  }

  /*
   * Return an input stream and offset corresponding to the logical index 'start'.
   * Offset refers to the number of bytes inside the physical resource that this streams starts at.
   */
  def getInputStream(start: Long) = {
    if (start >= size) throw new IndexOutOfBoundsException("Cannot load stream at pos " + start + ", stream size is: " + size)

    val (fileIdx, offset) = findFileOffset(start)
    val path = files(fileIdx).getPath
    val byteStream = path.getFileSystem(conf).open(path)
    if (offset != 0) { // jump to next available new line (and valid char)
      if (byteStream.skip(offset-1) != (offset-1)) throw new IOException("Unable to skip desired bytes in file")
    }
    (byteStream, offset)
  }

  /* Set the line reader to a newline-aligned input stream corresponding to logical index 'start' */
  final def openAtNewLine(start: Long) {
    val (byteStream, offset) = getInputStream(start)
    reader = new LineReader(byteStream, delimiter.getOrElse(null))
    text = new Text
    pos = start
    if (offset != 0) { // jump to next available new line (and valid char)
      pos += (reader.readLine(text) - 1)
    }
  }

  /* Construct a copy of this DeliteFileStream, starting at logical index 'start' */
  final def openCopyAtNewLine(start: Long): DeliteFileStream = {
    val copy = new DeliteFileStream(conf, files, charset, delimiter)
    copy.openAtNewLine(start)
    copy
  }

  /* Read the next line */
  private def readLineInternal() {
    var length = reader.readLine(text)
    if (length == 0) {
      if (pos+1 > size) {
        text = null
        return
      }
      else {
        val (nextByteStream, offset) = getInputStream(pos+1)
        reader = new LineReader(nextByteStream, delimiter.getOrElse(null))
        length = reader.readLine(text)
        assert(length != 0, "Filesystem returned an invalid input stream for position " + (pos+1))
      }
    }
    pos += length
  }

  /* Read the next line and interpret it as a String */
  final def readLine(): String = {
    if (!charset.isDefined) throw new UnsupportedOperationException("readLine() cannot be called without a charset defined")

    readLineInternal()
    if (text eq null) null
    else new String(text.getBytes, 0, text.getLength, charset.get)
  }

  /* Read the next line and interpret it as bytes */
  final def readBytes(): Array[Byte] = {
    readLineInternal()
    if (text eq null) null
    else {
      val record = new Array[Byte](text.getLength)
      System.arraycopy(text.getBytes, 0, record, 0, text.getLength)
      record
    }
  }

  /* Close the DeliteFileStream */
  final def close(): Unit = {
    reader.close()
    reader = null
    text = null
  }
}
