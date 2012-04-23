package ppl.dsl.optigraph.datastruct.scala

/**
 * The assignment to these variable types can be deferred
 */
class Deferrable[T:Manifest](private val initValue: T) {
  
  /** Holds the latest assigned value */
  private var currValue: T = initValue
  
  /** Holds the latest deferred value */
  private var defValue: Option[T] = None
  
  /** Returns the currently assigned value */
  def value: T = currValue
  /** Sets the current value */
  def setValue(value: T) = { currValue = value }
  /** Defers the value from being assigned */
  def defer(value: T) = { defValue = Some(value) }
  /** Sets the latest deferred value as the current value
   *  No effect if no value was deferred since last assignment  
   */
  def assign = { 
    defValue match {
      case Some(value) => currValue = value; defValue = None
      case None => // do nothing
    }
  }
}