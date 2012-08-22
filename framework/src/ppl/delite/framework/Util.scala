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

  // better way to do this? manifest <:< comparisons seem to fail
  def isSubtype(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
    if ((x == cls) || x.getInterfaces().contains(cls)) true
    else if (x.getSuperclass() == null && x.getInterfaces().length == 0) false
    else {
      val superIsSub = if (x.getSuperclass() != null) isSubtype(x.getSuperclass(), cls) else false
      superIsSub || x.getInterfaces().exists(s=>isSubtype(s,cls))
    }
  }      
}