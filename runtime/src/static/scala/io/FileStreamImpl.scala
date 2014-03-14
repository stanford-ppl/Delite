package generated.scala.io

import java.io.{FileInputStream, File, IOException}
import java.nio.charset.Charset
import org.apache.hadoop.util.LineReader
import org.apache.hadoop.io.Text

/**
 * Creates a single logical file stream by concatenating the streams of an arbitrary number of physical files
 * The stream can be opened at any byte offset and it will automatically seek to the next full line
 * Currently supports UTF-8 and extended-ASCII charsets; should be extensible to any multi-byte charset that is self-synchronizing (e.g., UTF-16))
 */
final class FileStreamImpl(charset: Charset, paths: String*) {

  def this(paths: String*) = this(Charset.defaultCharset, paths:_*)

  {
    val dec = charset.newDecoder
    if (dec.maxCharsPerByte != 1f || dec.averageCharsPerByte != 1f)
      throw new IOException("Unsupported Charset: " + charset.displayName)
  }

  private[this] final val jfiles = paths flatMap { path =>
    val jPath = new File(path)
    if (jPath.isDirectory) jPath.listFiles
    else if (jPath.isFile) Array(jPath)
    else throw new IllegalArgumentException("Path " + path + " does not appear to be a valid file or directory")
  }

  final val size: Long = jfiles.map(_.length).sum

  final val numThreads: Int = ppl.delite.runtime.Config.numThreads

  //TODO: it would be so much nicer to just create thread-local instances of this class
  private[this] final val pad = 32
  private[this] final val allReader = new Array[LineReader](pad*numThreads)
  private[this] final val allIdx = new Array[Int](pad*numThreads)
  private[this] final val allPos = new Array[Long](pad*numThreads)
  private[this] final val allEnd = new Array[Long](pad*numThreads)
  private[this] final val allText = new Array[Text](pad*numThreads) 

  final def pos(idx: Int) = allPos(pad*idx)

  final def end(idx: Int) = allEnd(pad*idx)

  private def findFileOffset(start: Long) = {
    var offset = start
    var fileIdx = 0
    while (offset >= jfiles(fileIdx).length) {
      offset -= jfiles(fileIdx).length
      fileIdx += 1
    }
    (fileIdx, offset)
  }

  final def openAtNewLine(threadIdx: Int): Unit = { 
    var pos = threadIdx * size / numThreads
    allEnd(pad*threadIdx) = (threadIdx + 1) * size / numThreads
    val (fileIdx, offset) = findFileOffset(pos)
    val byteStream = new FileInputStream(jfiles(fileIdx))
    val reader = new LineReader(byteStream)
    val text = new Text
    if (offset != 0) { //jump to next avaible new line (and valid char)
      if (byteStream.skip(offset-1) != (offset-1)) throw new IOException("Unable to skip desired bytes in file")
      pos += (reader.readLine(text) - 1)
    }
    allPos(pad*threadIdx) = pos
    allIdx(pad*threadIdx) = fileIdx
    allReader(pad*threadIdx) = reader
    allText(pad*threadIdx) = text
  }

  final def readLine(idx: Int): String = {
    val line = allText(pad*idx)
    var length = allReader(pad*idx).readLine(line)
    while (length == 0) {
      allIdx(pad*idx) += 1
      if (allIdx(pad*idx) >= jfiles.length) return null else allReader(pad*idx).close()
      allReader(pad*idx) = new LineReader(new FileInputStream(jfiles(allIdx(pad*idx))))
      length = allReader(pad*idx).readLine(line)
    }
    allPos(pad*idx) += length
    new String(line.getBytes, 0, line.getLength, charset)
  }
  
  final def close(idx: Int): Unit = { 
    allReader(pad*idx).close()
    allReader(pad*idx) = null
    allText(pad*idx) = null
  }

}
