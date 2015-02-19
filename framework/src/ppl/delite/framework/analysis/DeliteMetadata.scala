package ppl.delite.framework.analysis

import scala.collection.immutable.HashMap

trait DeliteMetadata {
  class IllegalMergeException extends Exception("Attempted to merge incompatible metadata instances")
  class IllegalMeetException extends Exception("Attempted to meet incompatible metadata instances")

  // Defs use underscore prefix since some implementations require calling other forms of the
  // overloaded method, which is more annoying (need to directly call implicitly[Meetable[...]].func)
  // if both the type class definition and the templated function have the same name
  // This effectively does the same thing as using implicitly[...] but with less code 
  trait Meetable[T] {
    // Tests whether a and b are identical
    def _matches(a: T, b: T): Boolean
    // Tests whether a and b can be met successfully
    def _canMeet(a: T, b: T): Boolean
    // Meet a and b
    def _meet(a: T, b: T): T
    // Test if a is completely filled in (known)
    def _isComplete(a: T): Boolean
    // Test if rhs can be merged into lhs
    def _canMergeLeft(lhs: T, rhs: T): Boolean
    // Merge rhs into lhs, overwriting entries in lhs when possible
    def _mergeLeft(lhs: T, rhs: T): T
    // Primarily for debugging/pretty printing
    def _makeString(a: T, prefix: String): String
    def _multiLine(a: T): Boolean
  }
  def matches[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]]._matches(a,b)
  def canMeet[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]]._canMeet(a,b)
  def meet[T:Meetable](a: T, b: T): T = implicitly[Meetable[T]]._meet(a,b)
  def isComplete[T: Meetable](a: T): Boolean = implicitly[Meetable[T]]._isComplete(a)
  def canMergeLeft[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]]._canMergeLeft(a,b)
  def mergeLeft[T: Meetable](a: T, b: T): T = implicitly[Meetable[T]]._mergeLeft(a,b)
  def makeString[T: Meetable](a: T, prefix: String = "") = implicitly[Meetable[T]]._makeString(a,prefix)
  def multiLine[T: Meetable](a: T) = implicitly[Meetable[T]]._multiLine(a)

  implicit def OptionCanBeMeetable[T: Meetable]: Meetable[Option[T]] = new Meetable[Option[T]] {
    def _matches(a: Option[T], b: Option[T]) = (a,b) match {
      case (Some(a),Some(b)) => matches(a,b)
      case (Some(_), None) => false
      case (None, Some(_)) => false
      case (None,None) => true
    }
    def _canMeet(a: Option[T], b: Option[T]): Boolean = (a,b) match {
      case (Some(am), Some(bm)) => canMeet(am,bm)
      case (None, Some(_)) => true
      case (Some(_), None) => true
      case (None, None) => true
    }
    def _meet(a: Option[T], b: Option[T]): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) if canMeet(am,bm) => Some(meet(am,bm))
      case (None, Some(bm)) => Some(bm)
      case (Some(am), None) => Some(am)
      case (None, None) => None
      case _ => throw new IllegalMeetException
    }
    def _isComplete(a: Option[T]): Boolean = a match {
      case Some(am) => isComplete(am)
      case None => false
    }
    def _canMergeLeft(a: Option[T], b: Option[T]): Boolean = (a,b) match {
      case (Some(am),Some(bm)) => canMergeLeft(a,b)
      case _ => true
    }
    def _mergeLeft(a: Option[T], b: Option[T]): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) => Some(mergeLeft(am,bm))
      case (Some(am), None) => Some(am)
      case (None, Some(bm)) => Some(bm)
      case (None, None) => None
    }
    def _makeString(a: Option[T], prefix: String): String = a match {
      case Some(am) => makeString(am, prefix)
      case None => " is unknown!"
    }
    def _multiLine(a: Option[T]): Boolean = a match {
      case Some(am) => multiLine(am)
      case None => false
    }
  }

  /**
   * New Metadata types should extend this abstract class
   * and add cases to metadataMatches, canMeetMetadata, meetMetadata, metadataIsComplete
   *
   * TODO: Should there be 3 child classes of Metadata for each symbol type?
   * Assuming here that there is metadata across symbol types, is this true?
   */ 
  abstract class Metadata { 
    def name: String
    //Test if this metadata instance has been sufficiently filled in
    def isComplete: Boolean = true

    // Pretty printing metadata (mostly for debugging)
    def makeString(prefix: String): String = this.toString()
    def multiLine = false
  }

  /**
   * Tests if this and that are identical (have no differences)
   * returns true if they are identical, false otherwise
   */
  def metadataMatches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case _ => false
  }
  /**
   * Test if this and that can be met to produce valid metadata
   * returns true if the two definitely can be met successfully
   */
  def canMeetMetadata(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case _ => false
  }
  /**
   * Meet this and that. If canMeet returns false for this and that,
   * meet should throw an exception, since the meet is invalid
   */
  def meetMetadata(a: Metadata, b: Metadata): Metadata = (a,b) match {
    case _ => throw new IllegalMeetException
  } 

  implicit object MetadataIsMeetable extends Meetable[Metadata] {
    def _matches(a: Metadata, b: Metadata) = metadataMatches(a,b)
    def _canMeet(a: Metadata, b: Metadata) = canMeetMetadata(a,b)
    def _meet(a: Metadata, b: Metadata) = meetMetadata(a,b)
    def _isComplete(a: Metadata) = a.isComplete
    def _canMergeLeft(a: Metadata, b: Metadata) = true
    def _mergeLeft(a: Metadata, b: Metadata) = a
    def _makeString(a: Metadata, prefix: String) = " = " + a.makeString(prefix)
    def _multiLine(a: Metadata) = a.multiLine
  }

  case class PropertyMap[T:Meetable](__info: Iterable[(String, Option[T])]) {
    def this(datum: (String, Option[T])) { this(Seq(datum)) }
    def this(info: PropertyMap[T]) { this(info.toList) }
    def this() { this(Nil) }

    val data = new HashMap[String, Option[T]] ++ __info

    def size: Int = data.size
    def apply(x: String): Option[T] = data.getOrElse(x, None)
    def contains(x: String): Boolean = data.contains(x)
    def toList: List[(String, Option[T])] = data.toList
    private def keys: Iterable[String] = data.keys

    def zip(that: PropertyMap[T])(f: (Option[T], Option[T]) => Option[T]): PropertyMap[T] = {
      val allKeys = this.keys ++ that.keys
      PropertyMap[T](allKeys zip allKeys.map{k => f(this(k), that(k))} )
    }

    /**
     * Reduce operation. Check that all properties in this map match the given condition
     * Trivially true if this contains no keys
     */
    def requireAll(f: Option[T] => Boolean): Boolean = {
      if (this.keys.isEmpty)
        true
      else
        this.keys.map{k => f(this(k))}.reduce{_&&_}
    }

    /**
     * Zip-Reduce operation. Get all keys from both maps, apply the zip function f which 
     * produces a boolean for every pair. Then reduce the booleans using the AND operation
     * Trivially true if neither this nor that contains any keys
     */
    def zipRequireAll(that: PropertyMap[T])(f: (Option[T], Option[T]) => Boolean): Boolean = {
      val allKeys = this.keys ++ that.keys
      if (allKeys.isEmpty) 
        true
      else
        allKeys.map{k => f(this(k), that(k)) }.reduce{_&&_}
    }
  }

  implicit def PropertyMapIsMeetable[T:Meetable]: Meetable[PropertyMap[T]] = new Meetable[PropertyMap[T]] {
    def _matches(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipRequireAll(b){(am,bm) => matches(am,bm)}
    def _canMeet(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipRequireAll(b){(am,bm) => canMeet(am,bm)}
    def _canMergeLeft(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipRequireAll(b){(am,bm) => canMergeLeft(am,bm)}
    def _isComplete(a: PropertyMap[T]): Boolean = a.requireAll{am => isComplete(am)}
    def _meet(a: PropertyMap[T], b: PropertyMap[T]): PropertyMap[T] = a.zip(b){(am,bm) => meet(am,bm)}
    def _mergeLeft(a: PropertyMap[T], b: PropertyMap[T]): PropertyMap[T] = a.zip(b){(am, bm) => mergeLeft(am,bm)}
  
    def _makeString(a: PropertyMap[T], prefix: String): String = {
      if (a.data.isEmpty) ""
      else 
        a.data.toList.sortBy{x => x._1}.map{dat => prefix + "." + dat._1 + makeString(dat._2, prefix)}.mkString("\n")
    }
    def _multiLine(a: PropertyMap[T]): Boolean = a.size > 0
  }

  /**
   * Parent class for metadata containers. Holds a hash map of 
   * string keys to single properties (Metadata instances)
   */ 
  sealed abstract class SymbolProperties (val data: PropertyMap[Metadata]) {
    def apply(x: String) = data(x)
  }

  // Metadata for scalar symbols (single element)
  case class ScalarProperties(override val data: PropertyMap[Metadata]) extends SymbolProperties(data)

  // Metadata for DeliteStructs 
  case class StructProperties(children: PropertyMap[SymbolProperties], override val data: PropertyMap[Metadata]) 
    extends SymbolProperties(data) 
  {
    def child(field: String) = children(field)
  }

  // Metadata for arrays
  case class ArrayProperties(child: Option[SymbolProperties] = None, override val data: PropertyMap[Metadata])
    extends SymbolProperties(data)

  // Test if properties for symbol type is complete
  // This needs to be overridden by metadata implementations / instances
  def dataComplete(a: SymbolProperties): Boolean

  implicit object SymbolPropertiesIsMeetable extends Meetable[SymbolProperties] {
    def _matches(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => 
        matches(a.data, b.data)
      case (a: StructProperties, b: StructProperties) => 
        matches(a.data,b.data) && matches(a.children, b.children)
      case (a: ArrayProperties, b: ArrayProperties) => 
        matches(a.data, b.data) && matches(a.child, b.child)
      case _ => false
    }
    def _canMeet(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => 
        canMeet(a.data, b.data)
      case (a: StructProperties, b: StructProperties) => 
        canMeet(a.data, b.data) && canMeet(a.children, b.children)
      case (a: ArrayProperties, b: ArrayProperties) =>
        canMeet(a.data, b.data) && canMeet(a.child, b.child)
      case _ => false
    }
    def _meet(a: SymbolProperties, b: SymbolProperties): SymbolProperties = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) if _canMeet(a,b) => 
        ScalarProperties(meet(a.data, b.data))
      case (a: StructProperties, b: StructProperties) if _canMeet(a,b)=>
        StructProperties(meet(a.children, b.children), meet(a.data, b.data))
      case (a: ArrayProperties, b: ArrayProperties) if _canMeet(a,b) =>
        ArrayProperties(meet(a.child, b.child), meet(a.data, b.data))
      case _ => throw new IllegalMeetException
    }
    def _isComplete(a: SymbolProperties): Boolean = a match {
      case a: ScalarProperties => isComplete(a.data) && dataComplete(a)
      case a: StructProperties => isComplete(a.data) && isComplete(a.children) && dataComplete(a)
      case a: ArrayProperties => isComplete(a.data) && isComplete(a.child) && dataComplete(a)
    }
    def _canMergeLeft(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => true
      case (a: StructProperties, b: StructProperties) => true
      case (a: ArrayProperties, b: ArrayProperties) => true
      case _ => false
    }
    def _mergeLeft(a: SymbolProperties, b: SymbolProperties): SymbolProperties = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) if _canMergeLeft(a,b) => 
        ScalarProperties(mergeLeft(a.data, b.data))
      case (a: StructProperties, b: StructProperties) if _canMergeLeft(a,b) => 
        StructProperties(mergeLeft(a.children, b.children), mergeLeft(a.data, b.data))
      case (a: ArrayProperties, b: ArrayProperties) if _canMergeLeft(a,b) =>
        ArrayProperties(mergeLeft(a.child, b.child), mergeLeft(a.data, b.data))
      case _ => throw new IllegalMergeException
    }

    def _makeString(a: SymbolProperties, prefix: String): String = a match { 
      case a: ScalarProperties => 
        ": Scalar {" + (if (multiLine(a.data)) "\n" else "") + makeString(a.data, prefix + "  ") + (if (multiLine(a.data)) "\n" + prefix else "") + "}"
      case a: StructProperties =>
        ": Struct {\n" + prefix + " Metadata:" + (if (multiLine(a.data)) "\n" + prefix else "")  + makeString(a.data, prefix + "  ") + "\n" +
                       prefix + " Fields:\n" + makeString(a.children, prefix + "  ") + "\n" + prefix + "}" 
      case a: ArrayProperties =>
        ": Array {\n" + prefix + " Metadata:" + (if (multiLine(a.data)) "\n" + prefix else "") + makeString(a.data, prefix + "  ") + "\n" +
                      prefix + " Child" + makeString(a.child, prefix + "  ") + "\n" + prefix + "}" 
    }
    def _multiLine(a: SymbolProperties): Boolean = a match {
      case a: ScalarProperties => multiLine(a.data)
      case _ => true
    }
  }

  object NoData extends PropertyMap[Metadata](Nil)
  object NoChildren extends PropertyMap[SymbolProperties](Nil)
}