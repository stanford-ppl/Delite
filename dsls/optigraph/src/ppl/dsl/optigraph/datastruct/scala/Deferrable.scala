package ppl.dsl.optigraph.datastruct.scala

/**
 * The assignment to these variable types can be deferred
 */
class Deferrable[T:Manifest](private val initValue: T) {

  /** Holds the latest assigned value */
  private var currValue: T = initValue

  /** Holds the latest deferred value */
  private var defValue: T = null.asInstanceOf[T]

  /** Indicates if defValue is relevant (we're deferring a value) */
  private var isDeferred = false

  /** Returns the currently assigned value */
  def value: T = currValue
  /** Sets the current value */
  def setValue(value: T) = { currValue = value }

  /** Sets the deferred value */
  def setDefValue(value: T) = { defValue = value }
  /** Returns the deferred value */
  def getDefValue : T = defValue
  /** Set the isDeferred boolean */
  def setDeferred(value: Boolean) = { isDeferred = value}
  /** Read the isDeferred boolean */
  def getDeferred: Boolean = isDeferred
}