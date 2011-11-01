package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/29/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class CRSConst(val values : Array[Int], val mult : Int) extends CRS {
  def row(i: Int) = {i * mult}
  def apply(row: Int, i: Int) = {values(row*mult + i)}
  def update(row: Int, i: Int, v: Int) {values(row*mult + i) = v}
  def len(row : Int) = mult
}
