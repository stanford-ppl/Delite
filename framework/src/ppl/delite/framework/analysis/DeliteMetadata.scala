// is this the right location for metadata base?
package ppl.delite.framework.datastructures

import scala.collection.immutable.HashMap
import ppl.delite.framework.analysis.Option

trait DeliteMetadata {
  class IllegalMetadataUpdateException extends Exception("Attempted to update a metadata instance with incompatible metadata")
  class IllegalMeetException extends Exception("Attempted to meet two incompatible metadata instances")
  class MetadataKeyException extends Exception("No key was found for a metadata instance when adding")

  /**
   * New Metadata types should extend this abstract class
   * and add cases to matches, canMeet, meet, isComplete, and metakey
   *
   * TODO: Should there be 3 child classes of Metadata for each symbol type?
   * Assuming here that there is metadata across symbol types, is this true?
   *
   * TODO: Do we also need a Properties type for DeliteArray? Should this be a 
   * common type with MultiArray (both only have one child)
   */ 
  abstract class Metadata { 
    // Pretty printing metadata (mostly for debugging)
    def makeString(prefix: String = ""): String = prefix + this.toString()
    def multiLine = false
  }

  /**
   * Tests if this and that are identical (have no differences)
   * returns true if they are identical, false otherwise
   */
  def matches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case _ => false
  }
  /**
   * Test if this and that can be met to produce valid metadata
   * returns true if the two definitely can be met successfully
   */
  def canMeet(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case _ => false
  }
  /**
   * Meet this and that. If canMeet returns false for this and that,
   * meet should throw an exception, since the meet is invalid
   */
  def meet(a: Metadata, b: Metadata): Metadata  = (a,b) match {
    case _ => throw new IllegalMeetException
  } 

  /**
   * Test if this metadata instance has been sufficiently filled in
   */
  def isComplete(a: Metadata): Metadata = a match {
    case _ => false
  }

  /**
   * Get key name based on type of metadata
   * Might make more sense as a mutable hashmap implementation?
   */ 
  def metakey(a: Metadata): String = a match {
    case _ => throw new MetadataKeyException
  }

  /**
   * Parent class for metadata containers. Holds a hash map of 
   * string keys to single properties (Metadata instances)
   *
   * TODO: change "SymbolProperties" to something more concise?
   */ 
  sealed abstract class SymbolProperties (
    __info: Iterable[(String, Option[Metadata])] = Nil
  ) {
    val data = new HashMap[String, Option[Metadata]] ++ __info

    def apply(x: String) = data.get(x)
    def keys = data.keys

    // Combines metadata entries from this and that using meet operation
    protected final def mergeData(that: SymbolProperties): Iterable[(String, Option[Metadata])] = {
      val allKeys = this.keys ++ that.keys 
      allKeys zip allKeys.map{k => meet(this(k), that(k)) }
    }
    // Combines metadata entries from this and that using update-like operation
    // Entry on LHS is replaced with corresponding RHS entry if RHS entry is not 'None'
    protected final def updateData(that: SymbolProperties): Iterable[(String, Option[Metadata])] = {
      val allKeys = this.keys ++ that.keys 
      allKeys zip allKeys.map{k => updateLHS(this(k), that(k)) }
    }

    // Tests whether all metadata entries are identical between this and that
    def matches(that: SymbolProperties): Boolean = {
      val allKeys = this.keys ++ that.keys
      allKeys.zip{k => matches(this(k), that(k))}.reduce{_&&_}
    }
    // Tests if this can be met with that successfully
    def canMeet(that: SymbolProperties): Boolean = {
      val allKeys = this.keys ++ that.keys
      allKeys.zip{k => canMeet(this(k), that(k))}.reduce{_&&_}
    }
    // Entry-wise meet of metadata fields (meet is defined per Metadata instance)
    def meet(that: SymbolProperties): SymbolProperties

    // Tests if this can be right dominant merged with that
    // (all of these are currently implemented with .isInstanceOf[...])
    def canBeUpdatedWith(that: SymbolProperties): Boolean
    
    // This right dominant merge with that
    // Metadata entries in this are replaced with corresponding entries in that
    // (unless the corresponding entry is unknown)
    def updateWith(that: SymbolProperties): SymbolProperties

    def makeString(prefix: String = ""): String = {
      data.toList.sorted map {dat => 
        prefix + dat._1 + ":" + (if (multiLine(dat._2)) "\n" else "") + makeString(dat._2, prefix + "  ")
      }.mkString("\n")
    }
  }

  /** 
   * Metadata for (native?) scalar symbols (single element)
   */
  case class ScalarProperties (
    __info: Iterable[(String, Option[Metadata])] = Nil
  ) extends SymbolProperties(__info) {

    override def matches(that: SymbolProperties): Boolean = {
      that.isInstanceOf[ScalarProperties] && super.equivalent(that)
    }
    override def canMeet(that: SymbolProperties): Boolean = {
      that.isInstanceOf[ScalarProperties] && super.compatible(that)
    }
    override def meet(that: SymbolProperties): SymbolProperties = {
      if (this canMeet that)
        ScalarProperties(this mergeData that)
      else 
        throw new IllegalMeetException
    }

    override def canBeUpdatedWith(that: SymbolProperties): Boolean = {
      that.isInstanceOf[ScalarProperties]
    }
    override def updateWith(that: SymbolProperties): SymbolProperties = {
      if (this canBeUpdatedWith that) 
        ScalarProperties(this updateData that)
      else
        throw new IllegalMetadataUpdateException
    }

    override def makeString(prefix: String = ""): String = {
      "Scalar {\n" + super.makeString(prefix + "  ") + "\n" + prefix + "}"
    }
  }

  /**  
   * Metadata for collections (holding multiple elements)
   */
  abstract class CollectionProperties (
    __info: Iterable[(String, Option[Metadata])]
  ) extends SymbolProperties(__info) 

  /**
   * Metadata for DeliteStructs
   */ 
  case class StructProperties (
    fieldMetadata: Iterable[(String, Option[SymbolProperties])] = Nil,
    __info: Iterable[(String, Option[Metadata])] = Nil
  ) extends CollectionProperties(__info) {
    val children = new HashMap[String,Option[SymbolProperties]] ++ fieldMetadata

    def fields = children.keys
    def child(field: String) = children.getOrElse(field, None)

    override def matches(that: SymbolProperties): Boolean = that match {
      case that: StructProperties if this.fields == that.fields =>
        super.matches(that) && fields.map{f => matches(this.child(f),that.child(f)) }.reduce{_&&_} 
      case _ => false 
    }
    override def canMeet(that: SymbolProperties): Boolean = that match {
      case that: StructProperties if this.fields == that.fields =>
        super.canMeet(that) && fields.map{f => canMeet(this.child(f),that.child(f)) }.reduce{_&&_}
      case _ => false
    }
    override def meet(that: SymbolProperties): SymbolProperties = that match {
      case that: StructProperties if this canMeet that => 
        val _fieldMetadata = fields zip fields.map{f => meet(this.child(f), that.child(f)) }
        StructProperties(_fieldMetadata, this mergeData that)
      case _ => throw new IllegalMeetException
    }

    override def canBeUpdatedWith(that: SymbolProperties): Boolean = {
      that.isInstanceOf[StructProperties]
    }
    override def updateWith(that: SymbolProperties): SymbolProperties = that match {
      case that: StructProperties =>
        val _fieldMetadata = fields zip fields.map{f => updateLHS(this.child(f), that.child(f)) } 
        StructProperties(_fieldMetadata, this updateData that)
      case _ => throw new IllegalMetadataUpdateException
    }

    override def makeString(prefix: String = ""): String = {
      val childStrings = children.toList.sorted map {c => 
        prefix + "  " + c._1 + " -> " + makeString(c._2, prefix + "  ")
      }.mkString("\n")

      "Struct {\n" + super.makeString(prefix + "  ") + "\n" + childStrings + prefix + "}"
    }
  }

  /**
   * Metadata for DeliteMultiArray
   */
  case class MultiArrayProperties (
    child: Option[SymbolProperties] = None,
    __info: Iterable[(String, Option[Metadata])] = Nil
  ) extends CollectionProperties(__info) {
    
    override def matches(that: SymbolProperties): Boolean = that match {
      case that: MultiArrayProperties => super.matches(that) && matches(this.child,that.child)
      case _ => false
    }
    override def canMeet(that: SymbolProperties): Boolean = that match {
      case that: MultiArrayProperties => super.canMeet(that) && canMeet(this.child,that.child)
      case _ => false
    }
    override def meet(that: SymbolProperties): SymbolProperties = that match {
      case that: MultiArrayProperties if this canMeet that =>
        MultiArrayProperties(meet(this.child,that.child), this mergeData that)
      case _ => throw new IllegalMeetException
    }

    override def canBeUpdatedWith(that: SymbolProperties): Boolean = {
      that.isInstanceOf[MultiArrayProperties]
    }
    override def updateWith(that: SymbolProperties): SymbolProperties = that match {
      case that: MultiArrayProperties => 
        MultiArrayProperties(updateLHS(this.child,that.child), this updateData that)
      case _ => throw new IllegalMetadataUpdateException
    }

    override def makeString(prefix: String = ""): String = {
      val childString = prefix + "  child =>  " + makeString(child, prefix + "  ")
      "MultiArray {\n" + super.makeString(prefix + "  ") + "\n" + childString + prefix + "}"
    }
  }

  /** 
   * Shortcuts for comparing Option[Metadata] and Option[SymbolProperties]
   *
   * Would be really nice to reduce code duplication here with a type class for
   * matches, canMeet, and meet operations, but Metadata is an abstract
   * class. meet() function returns type T (covariant) but getting an implementation of the
   * type class requires contravariance..
   */
  final def matches(a: Option[Metadata], b: Option[Metadata]): Boolean = (a,b) match {
    case (Some(am), Some(bm)) => matches(am,bm)
    case (None, None) => true
    case _ => false
  }  
  final def canMeet(a: Option[Metadata], b: Option[Metadata]): Boolean = (a,b) match {
    case (Some(am), Some(bm)) => canMeet(am,bm)
    case (None, Some(_)) => true
    case (Some(_), None) => true
    case (None, None) => true
  }
  //TODO: throwing an exception here is a bit ugly - better way to do this?
  final def meet(a: Option[Metadata], b: Option[Metadata]): Option[Metadata] = (a,b) match {
    case (Some(am), Some(bm)) if canMeet(am,bm) => Some(meet(am,bm))
    case (None, Some(bm)) => Some(bm)
    case (Some(am), None) => Some(am)
    case (None, None) => None
    case _ => throw new IllegalMeetException   // Shouldn't happen if compatible() is used
  }
  final def isComplete(a: Option[Metadata]): Boolean = a match {
    case Some(am) => am.isComplete
    case None => false
  }
  private def updateLHS(a: Option[Metadata], b: Option[Metadata]): Option[Metadata] = (a,b) match {
    case (Some(am), Some(bm)) => Some(bm)
    case (Some(am), None) => Some(am)
    case (None, Some(bm)) => Some(bm)
    case (None, None) => None
  }
  final def makeString(a: Option[Metadata], prefix: String = ""): String = a match {
    case Some(am) => am.makeString(prefix)
    case None => prefix + "??"
  }
  final def multiLine(a: Option[Metadata]): Boolean = a match {
    case Some(am) => am.multiLine
    case None => false
  }

  final def matches(a: Option[SymbolProperties], b: Option[SymbolProperties]): Boolean = (a,b) match {
    case (Some(am), Some(bm)) => am matches bm
    case (None, None) => true
    case _ => false
  }  
  final def canMeet(a: Option[SymbolProperties], b: Option[SymbolProperties]): Boolean = (a,b) match {
    case (Some(am), Some(bm)) => am canMeet bm
    case (None, Some(_)) => true
    case (Some(_), None) => true
    case (None, None) => true
  }
  //TODO: throwing an exception here is a bit ugly - better way to do this?
  final def meet(a: Option[SymbolProperties], b: Option[SymbolProperties]): Option[SymbolProperties] = (a,b) match {
    case (Some(am), Some(bm)) if am canMeet bm => Some(am meet bm)
    case (None, Some(bm)) => Some(bm)
    case (Some(am), None) => Some(am)
    case (None, None) => None
    case _ => throw new IllegalMeetException   // Shouldn't happen if compatible() is used
  }
  private def updateLHS(a: Option[SymbolProperties], b: Option[SymbolProperties]): Option[SymbolProperties] = (a,b) match {
    case (Some(am),Some(bm)) if am canBeUpdatedWith bm => Some(am updateWith bm)
    case (None, Some(bm)) => Some(bm)
    case (Some(am), None) => Some(am)
    case (None, None) => None
    case _ => throw new IllegalMetadataUpdateException
  }

  final def makeString(a: Option[SymbolProperties], prefix: String = ""): String = a match {
    case Some(am) => am.makeString(prefix)
    case None => prefix + "??"
  }
}