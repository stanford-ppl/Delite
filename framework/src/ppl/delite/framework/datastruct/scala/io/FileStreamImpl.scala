package ppl.delite.framework.datastruct.scala.io

import java.io.{BufferedReader, FileReader, File}

class FileStreamImpl(paths: String*) {

  private[this] val jfiles = paths flatMap { path =>
    val jPath = new File(path)
    if (jPath.isDirectory) jPath.listFiles
    else if (jPath.isFile) Array(jPath)
    else throw new IllegalArgumentException("Path " + path + " does not appear to be a valid file or directory")
  }

  val size: Long = jfiles.map(_.length).sum

  val numThreads: Int = ppl.delite.runtime.Config.numThreads

  private[this] val allReader = new Array[BufferedReader](numThreads)
  private[this] val allIdx = new Array[Int](numThreads)
  private[this] val allPos = new Array[Long](numThreads)
  private[this] val allEnd = new Array[Long](numThreads) 

  def pos(idx: Int) = allPos(idx)

  def end(idx: Int) = allEnd(idx)

  private def findFileOffset(start: Long) = {
    var offset = start
    var fileIdx = 0
    while (offset >= jfiles(fileIdx).length) {
      offset -= jfiles(fileIdx).length
      fileIdx += 1
    }
    (fileIdx, offset)
  }

  def openAtNewLine(threadIdx: Int): Unit = { 
    var pos = threadIdx * size / numThreads
    allEnd(threadIdx) = (threadIdx + 1) * size / numThreads
    val (fileIdx, offset) = findFileOffset(pos)
    val reader = new BufferedReader(new FileReader(jfiles(fileIdx)))
    if (offset != 0) {
      reader.skip(offset-1)
      pos += reader.readLine().length //+1-1 //TODO: need to handle /r/n files
    }
    allPos(threadIdx) = pos
    allIdx(threadIdx) = fileIdx
    allReader(threadIdx) = reader
  }

  def readLine(idx: Int): String = {
    var line = allReader(idx).readLine()
    while (line eq null) {
      allReader(idx).close()
      allIdx(idx) += 1
      if (allIdx(idx) >= jfiles.length) return null
      allReader(idx) = new BufferedReader(new FileReader(jfiles(allIdx(idx))))
      line = allReader(idx).readLine()
    }
    allPos(idx) += (line.length + 1) //TODO: need to handle /r/n files
    line
  }
  
  def close(idx: Int): Unit = { 
    allReader(idx).close()
    allReader(idx) = null
  }

}
