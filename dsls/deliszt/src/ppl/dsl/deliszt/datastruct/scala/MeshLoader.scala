package ppl.dsl.deliszt.datastruct.scala

import java.io._
import net.liftweb.json.JsonParser

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class MeshLoader {
  def init() {
    System.loadLibrary("MeshLoader");
    
    val cfg = BufferedReader(FileReader("liszt.cfg"))
    val json = JsonParser.parse(cfg)

    val meshFilename = json \ "mesh-file"
    Mesh.mesh = MeshLoader.loadMesh(meshFilename)
  }

  @native
  def loadMesh(file : String) : Mesh = null
}