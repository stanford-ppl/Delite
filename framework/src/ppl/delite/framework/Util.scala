package ppl.delite.framework

import java.io.File

object Util {

  def deleteDirectory(path: File) {
    if( path.exists) {
      path.listFiles.foreach{ f =>
        if(f.isDirectory) deleteDirectory(f)
        else f.delete
      }      
    }
    path.delete
  }

}