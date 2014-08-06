package generated.scala.io

import java.io.{FileInputStream, File, IOException}
import java.nio.charset.Charset
import org.apache.hadoop.util.LineReader
import org.apache.hadoop.io.Text
import com.google.protobuf.ByteString

/**
 * Creates a single logical file stream by concatenating the streams of an arbitrary number of physical files
 * The stream can be opened at any byte offset and it will automatically seek to the next full line
 * Currently supports UTF-8 and extended-ASCII charsets; should be extensible to any multi-byte charset that is self-synchronizing (e.g., UTF-16))
 * Also supports binary files via a user-provided custom record delimiter
 */

object FileStreamImpl {

  def apply(paths: Seq[String]): FileStreamImpl = FileStreamImpl(null, paths)
  def apply(charset: String, paths: Seq[String]): FileStreamImpl = new FileStreamImpl(checkCharset(charset), null, getFiles(paths))
  def bytes(delimiter: Array[Byte], paths: Seq[String]): FileStreamImpl = new FileStreamImpl(null, delimiter, getFiles(paths))

  private def getFiles(paths:Seq[String]) = paths.toArray flatMap { path =>
    val jPath = new File(path)
    if (jPath.isDirectory) {
      val files = jPath.listFiles
      scala.util.Sorting.quickSort(files) //numbered file chunks should be parsed in order
      files
    }
    else if (jPath.isFile) Array(jPath)
    else throw new IllegalArgumentException("Path " + path + " does not appear to be a valid file or directory")
  }

  private def checkCharset(charsetName: String) = {
    val charset = if (charsetName eq null) Charset.defaultCharset else Charset.forName(charsetName)
    val dec = charset.newDecoder
    if (dec.maxCharsPerByte != 1f || dec.averageCharsPerByte != 1f)
      throw new IOException("Unsupported Charset: " + charset.displayName)
    charset
  }

  def parseFrom(s: ByteString) = {
    val paths = s.toStringUtf8.split(",")
    apply(paths)
  }

}

final class FileStreamImpl(charset: Charset, delimiter: Array[Byte], jfiles: Array[File]) {

  private[this] var reader: LineReader = _
  private[this] var text: Text = _
  private[this] var idx: Int = _
  private[this] var pos: Long = _

  final val size: Long = jfiles.map(_.length).sum
  final def position = pos

  private def findFileOffset(start: Long) = {
    var offset = start
    var fileIdx = 0
    while (offset >= jfiles(fileIdx).length) {
      offset -= jfiles(fileIdx).length
      fileIdx += 1
    }
    (fileIdx, offset)
  }

  final def openCopyAtNewLine(start: Long): FileStreamImpl = {
    val copy = new FileStreamImpl(charset, delimiter, jfiles)
    copy.openAtNewLine(start)
    copy
  }

  final def openAtNewLine(start: Long) { 
    val (fileIdx, offset) = findFileOffset(start)
    val byteStream = new FileInputStream(jfiles(fileIdx))
    reader = new LineReader(byteStream, delimiter)
    text = new Text
    idx = fileIdx
    pos = offset
    if (offset != 0) { //jump to next avaible new line (and valid char)
      if (byteStream.skip(offset-1) != (offset-1)) throw new IOException("Unable to skip desired bytes in file")
      pos += (reader.readLine(text) - 1)
    }
  }

  final def readLine(): String = {
    val line = readLineInternal()
    if (line eq null) null
    else new String(line.getBytes, 0, line.getLength, charset)
  }

  final def readBytes(): Array[Byte] = {
    val line = readLineInternal()
    if (line eq null) null 
    else {
      val record = new Array[Byte](line.getLength)
      System.arraycopy(line.getBytes, 0, record, 0, line.getLength)
      record
    }
  }
  
  private def readLineInternal(): Text = {
    val line = text
    var length = reader.readLine(line)
    while (length == 0) {
      idx += 1
      if (idx >= jfiles.length) return null else reader.close()
      reader = new LineReader(new FileInputStream(jfiles(idx)), delimiter)
      length = reader.readLine(line)
    }
    pos += length
    line
  }
  
  final def close(): Unit = { 
    reader.close()
    reader = null
    text = null
  }

  final def toByteString = {
    ByteString.copyFromUtf8(jfiles.map(_.getAbsolutePath).mkString(","))
  }

}
