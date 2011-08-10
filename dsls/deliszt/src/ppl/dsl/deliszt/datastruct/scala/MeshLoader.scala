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

object MeshLoader {
  def init(cfgFile : String) {
    System.loadLibrary("MeshLoader");
    
    val cfg = new BufferedReader(new FileReader(cfgFile))
    val json = JsonParser.parse(cfg)

    implicit val formats = net.liftweb.json.DefaultFormats

    case class MeshFilename(`mesh-file`: String)

    val meshFilename = json.extract[MeshFilename].`mesh-file`
    Mesh.mesh = loadMesh(meshFilename)
  }

  @native
  def loadMesh(file : String) : Mesh = null

  def loadBoundaries[MO<:MeshObj:MeshObjConstruct](name : String) : MeshSet[MO] = {  
    _loadBoundaries(name)
  }

  @native
  def _loadBoundaries[MO<:MeshObj:MeshObjConstruct](name : String) : MeshSet[MO] = null
}