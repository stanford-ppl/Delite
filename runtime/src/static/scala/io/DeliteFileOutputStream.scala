package generated.scala.io

import generated.scala.ResourceInfo
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
  def apply(path: String, sequential: Boolean, resourceInfo: ResourceInfo): DeliteFileOutputStream = {
    val conf = new Configuration()
    val numFiles = 
      if (sequential) 1 
      else {
        if (resourceInfo.numSlaves == 0) resourceInfo.numThreads else resourceInfo.numSlaves*resourceInfo.numThreads
      }
    new DeliteFileOutputStream(conf, path, numFiles)
  }

  /* Deserialization for a DeliteFileOutputStream, using Hadoop serialization for Hadoop objects */
  def deserialize(bytes: ByteString): DeliteFileOutputStream = {
    val in = new java.io.DataInputStream(bytes.newInput)
    val conf = new Configuration()
    conf.readFields(in)
    val pathLen = in.readInt()
    val path = (Array.fill(pathLen) { in.readChar }).mkString
    val numFiles = in.readInt()
    new DeliteFileOutputStream(conf, path, numFiles)
  }
}

class DeliteFileOutputStream(conf: Configuration, path: String, numFiles: Int) {
  /* File writers are opened on demand so that distributed chunks do not interfere with one another. */
  private val paths: Array[Path] = getFilePaths(conf, path, numFiles)
  private val writers: Array[PrintWriter] = Array.fill[PrintWriter](numFiles) { null }

  /*
   * Construct paths for physical chunk files.
   * The input path must refer to a valid filesystem, based on the Hadoop configuration object.
   */
  private def getFilePaths(conf: Configuration, path:String, numFiles: Int) = {
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

    val paths = chunkPaths map { p => new Path(p) }
    paths.toArray
  }

  def getFileIdx(resourceInfo: ResourceInfo) = {
    if (numFiles == 1) 0
    else resourceInfo.slaveId*resourceInfo.numThreads + resourceInfo.threadId
  }

  /* Write the next line to a particular file chunk */
  def writeLine(resourceInfo: ResourceInfo, line: String) {
    val fileIdx = getFileIdx(resourceInfo)
    if (fileIdx < 0 || fileIdx >= writers.length) throw new IOException("chunk file index out of bounds")
    if (writers(fileIdx) == null) {
      val chunkPath = paths(fileIdx)
      val stream = chunkPath.getFileSystem(conf).create(chunkPath)
      writers(fileIdx) = new PrintWriter(new BufferedWriter(new OutputStreamWriter(stream)))
    }
    writers(fileIdx).println(line)
  }

  /* Close a writer in the stream associated with a particular file */
  def close(resourceInfo: ResourceInfo) {
    val fileIdx = getFileIdx(resourceInfo)
    if (writers(fileIdx) != null) {
      writers(fileIdx).close()
      writers(fileIdx) = null
    }
  }

  /*
   * Close the DeliteFileOutputStream
   *
   * Beware: if this is called on a single node in distributed mode, not all of
   * the writers are guaranteed to be flushed.
   */
  final def close(): Unit = (0 until writers.length) foreach { i =>
    if (writers(i) != null) { writers(i).close(); writers(i) = null }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Serialization
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Serialize the DeliteFileOutputStream using Hadoop serialization for Hadoop objects.
   */
  def serialize: ByteString = {
    val outBytes = ByteString.newOutput
    val out = new java.io.DataOutputStream(outBytes)
    conf.write(out)
    out.writeInt(path.length)
    out.writeChars(path)
    out.writeInt(numFiles)
    outBytes.toByteString
  }

  /* Deserialization is static (see DeliteFileOutputStream.deserialize) */
}
