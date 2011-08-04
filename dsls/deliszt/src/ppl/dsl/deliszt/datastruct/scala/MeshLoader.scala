package ppl.dsl.deliszt.datastruct.scala

import java.io._
import net.liftweb.json.{JsonParser, JsonAST}
import net.liftweb.json.JsonDSL._

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
    
    val cfg = new BufferedReader(new FileReader("liszt.cfg"))
    val json = JsonParser.parse(cfg)

    implicit val formats = net.liftweb.json.DefaultFormats

    case class MeshFilename(`mesh-file`: String)

    val meshFilename = json.extract[MeshFilename].`mesh-file`
    Mesh.mesh = loadMesh(meshFilename)
  }

  @native
  def loadMesh(file : String) : Mesh = null

  def loadBoundaries[MO<:MeshObj:MeshObjConstruct](name : String) : MeshSet[MO] = {
    val bs = new BoundarySetImpl[MO]
    _loadBoundaries(name, bs)
    bs
  }

  @native
  def _loadBoundaries[MO<:MeshObj:MeshObjConstruct](name : String, bs : BoundarySetImpl[MO]) : MeshSet[MO] = null
}