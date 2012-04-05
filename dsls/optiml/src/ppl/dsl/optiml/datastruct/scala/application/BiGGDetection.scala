package ppl.dsl.optiml.datastruct.scala
import ppl.dsl.optiml.datastruct.scala._

class BiGGDetection (
  val name: String,
  val score: Float,
  val roi: Rect,
  val mask: Image[Int],
  val index: Int,
  val x: Int,
  val y: Int,
  val tpl: BinarizedGradientTemplate,
  val crt_tpl: BinarizedGradientTemplate
) 
