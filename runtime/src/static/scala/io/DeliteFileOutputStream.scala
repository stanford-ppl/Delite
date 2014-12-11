package generated.scala.io

import com.google.protobuf.ByteString
import java.io.{PrintWriter, BufferedWriter, OutputStreamWriter, IOException}

import org.apache.hadoop.conf._
import org.apache.hadoop.fs._

/**
 * Writes lines to a logical file stream by breaking the stream up into multiple physical files.
 * The destination path can be backed by a different Hadoop-supported filesystem (inclues local, HDFS and S3).
 */
object DeliteFileOutputStream {

  /* Construct a new DeliteFileOutputStream */
  def apply(path: String, numFiles: Int): DeliteFileOutputStream = {
    val conf = new Configuration()
    new DeliteFileOutputStream(conf, path, numFiles)
  }
}

class DeliteFileOutputStream(conf: Configuration, path: String, numFiles: Int) {  
  private val writers: Array[PrintWriter] = getFileWriters(conf, path, numFiles)

  /* 
   * Open file streams to the physical chunk files.
   * The input path must refer to a valid filesystem, based on the Hadoop configuration object.
   */
  private def getFileWriters(conf: Configuration, path:String, numFiles: Int) = {
    // We assume the logical file starts at physical index 0 and at ends at physical index numFiles-1
    // The chunk files are written to a directory at 'path', with each chunk inside this output directory.
    // The output can be reconstructed by alphanumerically sorting the chunks, which is what DeliteFileInputStream does.
    val chunkPaths = 
      if (numFiles > 1) {
        val basePath = new Path(path)    
        basePath.getFileSystem(conf).mkdirs(basePath)
        (0 until numFiles) map { i => path + Path.SEPARATOR + basePath.getName() + "_" + "%04d".format(i) }
      }    
      else {
        path :: Nil
      }

    val writers = chunkPaths map { p =>
      val chunkPath = new Path(p)
      val stream = chunkPath.getFileSystem(conf).create(chunkPath)
      new PrintWriter(new BufferedWriter(new OutputStreamWriter(stream)))
    }
    writers.toArray
  }

  /* Write the next line to a particular file chunk */
  def writeLine(fileIdx: Int, line: String) {
    if (fileIdx < 0 || fileIdx >= writers.length) throw new IOException("chunk file index out of bounds")
    writers(fileIdx).println(line)
  }

  /* Close the DeliteFileOutputStream */
  final def close(): Unit = writers foreach { _.close() }
}
