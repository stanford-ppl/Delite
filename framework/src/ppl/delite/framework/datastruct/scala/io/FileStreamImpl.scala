package ppl.delite.framework.datastruct.scala.io

import java.io.{BufferedReader, FileReader, File}

final class FileStreamImpl(paths: String*) {

  private[this] final val jfiles = paths flatMap { path =>
    val jPath = new File(path)
    if (jPath.isDirectory) jPath.listFiles
    else if (jPath.isFile) Array(jPath)
    else throw new IllegalArgumentException("Path " + path + " does not appear to be a valid file or directory")
  }

  final val size: Long = jfiles.map(_.length).sum

  final val numThreads: Int = ppl.delite.runtime.Config.numThreads

  private[this] final val pad = 32
  private[this] final val allReader = new Array[BufferedReader](pad*numThreads)
  private[this] final val allIdx = new Array[Int](pad*numThreads)
  private[this] final val allPos = new Array[Long](pad*numThreads)
  private[this] final val allEnd = new Array[Long](pad*numThreads) 

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
    val reader = new BufferedReader(new FileReader(jfiles(fileIdx)))
    if (offset != 0) {
      reader.skip(offset-1)
      pos += reader.readLine().length //+1-1 //TODO: need to handle /r/n files
    }
    allPos(pad*threadIdx) = pos
    allIdx(pad*threadIdx) = fileIdx
    allReader(pad*threadIdx) = reader
  }

  final def readLine(idx: Int): String = {
    var line = allReader(pad*idx).readLine()
    while (line eq null) {
      allReader(pad*idx).close()
      allIdx(pad*idx) += 1
      if (allIdx(pad*idx) >= jfiles.length) return null
      allReader(pad*idx) = new BufferedReader(new FileReader(jfiles(allIdx(pad*idx))))
      line = allReader(pad*idx).readLine()
    }
    allPos(pad*idx) += (line.length + 1) //TODO: need to handle /r/n files
    line
  }
  
  final def close(idx: Int): Unit = { 
    allReader(pad*idx).close()
    allReader(pad*idx) = null
  }

}
