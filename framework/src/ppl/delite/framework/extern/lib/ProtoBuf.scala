package ppl.delite.framework.extern.lib

import ppl.delite.framework.Config

object ProtoBuf extends ExternalLibrary {
  val libName = "protobuf"
  val configFile = "protobuf.xml"
  val ext = "proto"
  val libExt = "java"
  // should we consider library linking machine dependent? do we have a different external lib
  // for unix and windows?

  val runtimePath=Config.homeDir + sep + Array("runtime", "src", "ppl", "delite", "runtime", "messages").mkString(sep)
  val structsPath=Config.buildDir + sep + Array("scala", "datastructures").mkString(sep)

  val compileFlags = List()
  val outputSwitch = "--java_out="

  override lazy val headerDir = List("-I"+structsPath, "-I"+runtimePath) ++ config.headerDir
  override lazy val destFileName = ""
  override val separateOutput = false

}
