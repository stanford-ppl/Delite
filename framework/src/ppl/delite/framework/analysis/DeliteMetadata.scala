package ppl.delite.framework.analysis

import scala.collection.immutable.HashMap

trait DeliteMetadata {
  class IllegalMergeException extends Exception("Attempted to merge incompatible metadata instances")
  class IllegalMeetException extends Exception("Attempted to meet incompatible metadata instances")
  class MetadataKeyException extends Exception("No key was found for a metadata instance when adding")

  trait Meetable[T] {
    // Tests whether a and b are identical
    def matches(a: T, b: T): Boolean
    // Tests whether a and b can be met successfully
    def canMeet(a: T, b: T): Boolean
    // Meet a and b
    def meet(a: T, b: T): T
    // Test if a is completely filled in (known)
    def isComplete(a: T): Boolean
    // Test if rhs can be merged into lhs
    def canMergeLeft(lhs: T, rhs: T): Boolean
    // Merge rhs into lhs, overwriting entries in lhs when possible
    def mergeLeft(lhs: T, rhs: T): T
    // Primarily for debugging/pretty printing
    def makeString(a: T, prefix: String): String
    def multiLine(a: T): Boolean
  }
  def matches[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]].matches(a,b)
  def canMeet[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]].canMeet(a,b)
  def meet[T:Meetable](a: T, b: T): T = implicitly[Meetable[T]].meet(a,b)
  def isComplete[T: Meetable](a: T): Boolean = implicitly[Meetable[T]].isComplete(a)
  def canMergeLeft[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]].canMergeLeft(a,b)
  def mergeLeft[T: Meetable](a: T, b: T): T = implicitly[Meetable[T]].mergeLeft(a,b)
  def makeString[T: Meetable](a: T, prefix: String = "") = implicitly[Meetable[T]].makeString(a,prefix)
  def multiLine[T: Meetable](a: T) = implicitly[Meetable[T]].multiLine(a)

  implicit def OptionCanBeMeetable[T: Meetable]: Meetable[Option[T]] = new Meetable[Option[T]] {
    lazy val meeter = implicitly[Meetable[T]]

    def matches(a: Option[T], b: Option[T]) = (a,b) match {
      case (Some(a),Some(b)) => meeter.matches(a,b)
      case (Some(_), None) => false
      case (None, Some(_)) => false
      case (None,None) => true
    }
    def canMeet(a: Option[T], b: Option[T]): Boolean = (a,b) match {
      case (Some(am), Some(bm)) => meeter.canMeet(am,bm)
      case (None, Some(_)) => true
      case (Some(_), None) => true
      case (None, None) => true
    }
    def meet(a: Option[T], b: Option[T]): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) if meeter.canMeet(am,bm) => Some(meeter.meet(am,bm))
      case (None, Some(bm)) => Some(bm)
      case (Some(am), None) => Some(am)
      case (None, None) => None
      case _ => throw new IllegalMeetException
    }
    def isComplete(a: Option[T]): Boolean = a match {
      case Some(am) => meeter.isComplete(am)
      case None => false
    }
    def mergeLeft(a: Option[T], b: Option[T]): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) => meeter.mergeLeft(am,bm)
      case (Some(am), None) => Some(am)
      case (None, Some(bm)) => Some(bm)
      case (None, None) => None
    }
    def makeString(a: Option[T], prefix: String): String = a match {
      case Some(am) => meeter.makeString(a, prefix)
      case None => prefix + "??"
    }
    def multiLine(a: Option[T]): Boolean = a match {
      case Some(am) => meeter.multiLine(am)
      case None => false
    }
  }

  /**
   * New Metadata types should extend this abstract class
   * and add cases to metadataMatches, canMeetMetadata, meetMetadata, metadataIsComplete
   *
   * TODO: Should there be 3 child classes of Metadata for each symbol type?
   * Assuming here that there is metadata across symbol types, is this true?
   *
   * TODO: Do we also need a Properties type for DeliteArray? Should this be a 
   * common type with MultiArray (both only have one child)
   */ 
  abstract class Metadata { 
    val name: String
    //Test if this metadata instance has been sufficiently filled in
    def isComplete: Boolean = true

    // Pretty printing metadata (mostly for debugging)
    def makeString(prefix: String = ""): String = prefix + this.toString()
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
    def matches(a: Metadata, b: Metadata) = metadataMatches(a,b)
    def canMeet(a: Metadata, b: Metadata) = canMeetMetadata(a,b)
    def meet(a: Metadata, b: Metadata) = meetMetadata(a,b)
    def isComplete(a: Metadata) = metadataIsComplete(a)
    def canMergeLeft(a: Metadata, b: Metadata) = true
    def mergeLeft(a: Metadata, b: Metadata) = a
    def makeString(a: Metadata, prefix: String) = a.makeString(prefix)
    def multiLine(a: Metadata) = a.multiLine
  }

  // TODO: Can this extend HashMap and have a non-empty constructor?
  case class PropertyMap[T:Meetable](__info: Iterable[(String, Option[T])]) {
    def this(datum: (String, Option[T])) { this(Seq(datum)) }
    def this(info: PropertyMap[T]) { this(info.toList) }
    def this() { this(Nil) }

    val data = new HashMap[String, Option[T]] ++ __info

    def size = data.size
    def apply(x: String) = data.get(x)
    def contains(x: String) = data.contains(x)
    def toList = data.toList
    private def keys = data.keys

    def andAll(f: Option[T] => Boolean): Boolean = this.keys.map{k => f(this(k))}.reduce{_&&_}

    def zip(that: PropertyMap[T])(f: (Option[T], Option[T]) => Option[T]): PropertyMap[T] = {
      val allKeys = this.keys ++ that.keys
      PropertyMap[T](allKeys zip allKeys.map{k => f(this(k), that(k))} )
    }
    def zipAndAll(that: PropertyMap[T])(f: (Option[T], Option[T]) => Boolean): Boolean = {
      val allKeys = a.keys ++ b.keys
      allKeys.zip{k => f(this(k), that(k)) }.reduce{_&&_}
    }
  }
  object NoData extends PropertyMap[Metadata](Nil)
  object NoChildren extends PropertyMap[SymbolProperties](Nil)


  // TODO: Calls "new" on every invocation :(
  implicit def PropertyMapIsMeetable[T:Meetable]: Meetable[PropertyMap[T]] = new Meetable[PropertyMap[T]] {
    lazy val meeter = implicitly[Meetable[T]]

    def matches(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipAndAll(b){(am,bm) => meeter.matches(am,bm)}
    def canMeet(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipAndAll(b){(am,bm) => meeter.canMeet(am,bm)}
    def canMergeLeft(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipAndAll(b){(am,bm) => meeter.canMergeLeft(am,bm)}
    def isComplete(a: PropertyMap[T]): Boolean = a.andAll{am => meeter.isComplete(am)}
    def meet(a: PropertyMap[T], b: PropertyMap[T]): PropertyMap[T] = a.zip(b){(am,bm) => meeter.meet(am,bm)}
    def mergeLeft(a: PropertyMap[T], b: PropertyMap[T]): PropertyMap[T] = a.zip(b){(am, bm) => meeter.mergeLeft(am,bm)}
  
    def makeString(a: PropertyMap[T], prefix: String): String = {
      if (a.data.isEmpty) ""
      else 
        a.data.toList.sorted map {dat => 
          prefix + dat._1 + ":" + (if (meeter.multiLine(dat._2)) "\n" else "") + meeter.makeString(data._2, prefix + "  ")
        }.mkString("\n")
    }
    def multiLine(a: PropertyMap[T]): Boolean = a.size > 1
  }

  /**
   * Parent class for metadata containers. Holds a hash map of 
   * string keys to single properties (Metadata instances)
   *
   * TODO: change "SymbolProperties" to something more concise?
   */ 
  sealed abstract class SymbolProperties (val data: PropertyMap[Metadata]) {
    def apply(x: String) = data(x)
  }

  // Metadata for (native?) scalar symbols (single element)
  case class ScalarProperties(data: PropertyMap[Metadata]) extends SymbolProperties(data)

  // Metadata for collections (holding multiple elements)
  // TODO: Is this needed?
  abstract class CollectionProperties(data: PropertyMap[Metadata]) extends SymbolProperties(data) 

  // Metadata for DeliteStructs 
  case class StructProperties(children: PropertyMap[SymbolProperties], data: PropertyMap[Metadata]) 
    extends CollectionProperties(data) 
  {
    def child(field: String) = children(field)
  }

  // Metadata for arrays
  case class ArrayProperties(child: Option[SymbolProperties] = None, data: PropertyMap[Metadata])
    extends CollectionProperties(data)

  // These will need to be overridden by analysis stages
  def dataComplete(a: ScalarProperties): Boolean = true
  def dataComplete(a: StructProperties): Boolean = true
  def dataComplete(a: ArrayProperties): Boolean = true

  implicit object SymbolPropertiesIsMeetable extends Meetable[SymbolProperties] {
    def matches(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => 
        a.dataMatches(b)
      case (a: StructProperties, b: StructProperties) => 
        a.fields == b.fields && canMeet(a.data,b.data) && 
      case (a: ArrayProperties, b: ArrayProperties) => 
        a.dataMatches(b) && matches(a.child, b.child)
      case _ => false
    }
    def canMeet(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => 
        a.dataCanMeet(b)
      case (a: StructProperties, b: StructProperties) => 
        a.dataCanMeet(b) && a.fields.map{f => canMeet(a.child(f), b.child(f)) }.reduce{_&&_}
      case (a: ArrayProperties, b: ArrayProperties) =>
        a.dataCanMeet(b) && canMeet(a.child, b.child)
      case _ => false
    }
    def meet(a: SymbolProperties, b: SymbolProperties): SymbolProperties = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) if canMeet(a,b) => 
        ScalarProperties(a.meetData(b))
      case (a: StructProperties, b: StructProperties) if canMeet(a,b)=> 
        val _fieldMetadata = a.fields zip a.fields.map{f => meet(a.child(f), b.child(f)) }
        StructProperties(_fieldMetadata, a.meetData(b))
      case (a: ArrayProperties, b: ArrayProperties) if canMeet(a,b) =>
        ArrayProperties(meet(a.child,b.child), a.meetData(b))
      case _ => throw new IllegalMeetException
    }
    def isComplete(a: SymbolProperties): SymbolProperties = a match {
      case a: ScalarProperties => isComplete(a.data) && dataComplete(a)
      case a: StructProperties => isComplete(a.data) && isComplete(a.children) && dataComplete(a)
      case a: ArrayProperties => isComplete(a.data) && isComplete(a.child) && dataComplete(a)
    }

    def canMergeLeft(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => true
      case (a: StructProperties, b: StructProperties) => true
      case (a: ArrayProperties, b: ArrayProperties) => true
      case _ => false
    }
    def mergeLeft(a: SymbolProperties, b: SymbolProperties): SymbolProperties = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) if canMergeLeft(a,b) => 
        ScalarProperties(a.updateData(b))
      case (a: StructProperties, b: StructProperties) if canMergeLeft(a,b) => 
        val _fieldMetadata = a.fields zip a.fields.map{f => mergeLeft(a.child(f), a.child(f)) } 
        StructProperties(_fieldMetadata, a.updateData(b))
      case (a: ArrayProperties, b: ArrayProperties) if canMergeLeft(a,b) =>
        ArrayProperties(mergeLeft(a.child, b.child), a.updateData(b))
      case _ => throw new IllegalMetadataUpdateException
    }

    def makeString(a: SymbolProperties, prefix: String): String = a match { 
      case a: ScalarProperties => 
        prefix + "Scalar {" + (if (multiLine(a.data)) "\n" else "") + makeString(a.data, prefix + "  ") + (if (multiLine(a.data)) "\n" + prefix else "") + "}"
      case a: StructProperties =>
        prefix + "Struct {" + prefix + " Metadata\n" + makeString(a.data, prefix + "  ") + "\n" + prefix + " Fields\n" + makeString(a.children, prefix + "  ") + "\n" + prefix + "}" 
      case a: ArrayProperties =>
        prefix + "Array {" + prefix + " Metadata\n" + makeString(a.data, prefix + "  ") + "\n" + prefix + " Child\n" + makeString(a.child, prefix + "  ") + "\n" + prefix + "}" 
    }
    def multiLine(a: SymbolProperties): Boolean = a match {
      case a: ScalarProperties => multiLine(a.data)
      case _ => true
    }
  }
}