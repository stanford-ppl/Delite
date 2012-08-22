package ppl.dsl.deliszt.datastruct.scala

import java.io._
import scala.util.parsing.json._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MeshLoader {
  var loaded = false

  def init(cfgPath : String) {
    Global.wall_start = System.currentTimeMillis
  
    if(!loaded) {    
      try {
        System.loadLibrary("MeshLoader");
      }
      catch {
        case e: java.lang.UnsatisfiedLinkError => if(e.getMessage.indexOf("already loaded") < 0) throw e
      }

      loaded = true
    }

    val cfgFile = new File(cfgPath)

    if(!cfgFile.exists) {
      throw new FileNotFoundException("Liszt cfg file " + cfgPath + " does not exist")
    }
    
    val cfgSrc = scala.io.Source.fromFile(cfgFile)
    val cfg = cfgSrc.getLines.mkString
    cfgSrc.close()
    val json = JSON.parseFull(cfg)

    Mesh.loader = new MeshLoader()

    val meshFilename = json.get.asInstanceOf[Map[String,Any]].get("mesh-file").get.asInstanceOf[String]

    println("Loading mesh file " + meshFilename)
    
    var file = new File(meshFilename)
    if(!file.exists()) {
      val resource = getClass.getResource(meshFilename)

      if(resource != null) {
        file = new File(resource.getPath)
      }
    }

    if(!file.exists()) {
      file = new File(cfgFile.getParent, meshFilename)
    }

    if(file.exists()) {
      println("File exists, found at " + file.getPath)
      Mesh.mesh = Mesh.loader.loadMesh(file.getPath)
      
      println("ncells: " + Mesh.mesh.ncells)
      println("nedges: " + Mesh.mesh.nedges)
      println("nfaces: " + Mesh.mesh.nfaces)
      println("nvertices: " + Mesh.mesh.nvertices)
    }
    else {
      throw new FileNotFoundException("Mesh file " + meshFilename + " does not exist")
    }
  }
}

class MeshLoader {
  @native
  def loadMesh(file: String) : Mesh = null

  def loadBoundarySet(name: String, mo_type: Int) = {
    val bs = _loadBoundarySet(name, mo_type)
    
    if(bs == null) {
      throw new RuntimeException("Loading boundary set " + name + " of type " + mo_type + " failed!")
    }
    
    bs
  }
  
  @native
  def _loadBoundarySet(name: String, mo_type: Int) : BoundarySet = null
}
