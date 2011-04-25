package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 03/29/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class CRS(_rows : Array[Int], _values : Array[Int]) {
  def row(r : Int) = {_rows(r)}
  def apply(row : Int, i : Int) = {_values(_rows(row) + i)}
  def update(row : Int, i : Int, v : Int) = {_values(_rows(row) + i) = v}
  def len(row : Int) = {_rows(row+1) - _rows(row)}
}