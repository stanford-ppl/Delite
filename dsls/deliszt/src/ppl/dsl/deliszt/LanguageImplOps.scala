package ppl.dsl.deliszt

import ppl.dsl.deliszt.datastruct.scala._

trait LanguageImplOps {
  this: DeLiszt =>
  def DeLisztInit() : Unit = {
    // Load cfg files
    //
    MeshLoader.init()

    val cfg = BufferedReader(FileReader("liszt.cfg"))
    val json = JsonParser.parse(cfg)

    val meshFilename = json \ "mesh-file"
    Mesh.mesh = MeshLoader.loadMesh(meshFilename)
  }
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: DeLiszt =>
}
