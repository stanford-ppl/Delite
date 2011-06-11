package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/29/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait CRS {
  def row(r : Int) : Int
  def apply(row : Int, i : Int) : Int
  def update(row : Int, i : Int, v : Int) : Unit

  def apply(e : MeshObj, i : Int) = apply(e.internalId, i)
  def update(e : MeshObj, i : Int, v : Int) = update(e.internalId, i, v)

  def len(row : Int) : Int
  def values : Array[Int]
}

class CRSImpl(val rows : Array[Int], val values : Array[Int]) extends CRS {
  def row(r : Int) = {_rows(r)}
  def apply(row : Int, i : Int) = {_values(_rows(row) + i)}
  def update(row : Int, i : Int, v : Int) = {_values(_rows(row) + i) = v}
  def len(row : Int) = {_rows(row+1) - _rows(row)}
}