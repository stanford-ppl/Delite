package ppl.dsl.optigraph.datastruct.scala

/**
 * Can be used in a parallel context
 */
class Reduceable[T:Manifest](private val initValue: T) {
  
  /** Holds the current value of the reduceable */
  private var currValue: T = initValue

  def getValue():T = currValue
  def setValue(v: T) = { currValue = v }
  
  // called implicitly at the end of a parallel iteration block
  def combine(op: (T,T)=>T, v: T) = { currValue = op(currValue, v) } 
}