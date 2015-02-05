// is this the right location for metadata base?
package ppl.delite.framework.datastructures

import scala.collection.immutable.HashMap
import ppl.delite.framework.analysis.Slot

trait DeliteMetadata {
  class IllegalMeetException extends Exception("Attempted to meet two incompatible metadata instances")

  /**
   * New Metadata types should extend this
  */ 
  abstract class Metadata { 
    /**
     * Tests if this and that are identical (have no differences)
     * returns true if they are identical, false otherwise
    */
    def matches(that: Metadata): Boolean
    /**
     * Test if this and that can be met to produce valid metadata
     * returns true if the two definitely can be met successfully
    */
    def canMeet(that: Metadata): Boolean
    /**
     * Meet this and that. If canMeet returns false for this and that,
     * meet should throw an exception, since the meet is invalid
    */
    def meet(that: Metadata): Metadata    // should be Option[Metadata]? 
  }

  /**
   * Parent class for metadata containers. Holds a hash map of 
   * string keys to single properties (Metadata instances)
   * TODO: change "SymbolProperties" to something more concise
  */ 
  sealed abstract class SymbolProperties (
    __info: Iterable[(String, Slot[Metadata])] = Nil
  ) {
    val data = new HashMap[String, Slot[Metadata]] ++ __info

    def apply(x: String) = info.getOrElse(x, Unknown)
    def keys = info.keys

    protected final def mergeData(that: SymbolProperties): Iterable[(String, Slot[Metadata])] = {
      val allKeys = this.data.keys ++ that.data.keys 
      allKeys zip allKeys.map{k => meet(this(k), that(k)) }
    }

    def matches(that: SymbolProperties): Boolean = {
      val allKeys = this.data.keys ++ that.data.keys
      allKeys.zip{k => matches(this(k), that(k))}.reduce{_&&_}
    }
    def canMeet(that: SymbolProperties): Boolean = {
      val allKeys = this.data.keys ++ that.data.keys
      allKeys.zip{k => canMeet(this(k), that(k))}.reduce{_&&_}
    }
    def meet(that: SymbolProperties): SymbolProperties
  }

  /** 
   * Metadata for (native?) scalar symbols (single element)
  */
  case class ScalarProperties (
    __info: Iterable[(String, Slot[Metadata])] = Nil
  ) extends SymbolProperties(__info) {

    override def matches(that: SymbolProperties): Boolean = {
      that.isInstanceOf[ScalarProperties] && super.equivalent(that)
    }
    override def canMeet(that: SymbolProperties): Boolean = {
      that.isInstanceOf[ScalarProperties] && super.compatible(that)
    }
    override def meet(that: SymbolProperties): SymbolProperties = {
      if (this canMeet that) {
        ScalarProperties(this mergeData that)
      }
      else throw new IllegalMeetException
    }
  }

  /**  
   * Metadata for collections (holding multiple elements)
  */
  abstract class CollectionProperties (
    __info: Iterable[(String, Slot[Metadata])] = Nil
  ) extends SymbolProperties(__info) 

  /**
   * Metadata for DeliteStructs
  */ 
  case class StructProperties (
    fieldMetadata: Iterable[(String, Slot[SymbolMetadata])] = Nil,
    __info: Iterable[(String, Slot[Metadata])] = Nil
  ) extends CollectionProperties(__info) {
    val children = new HashMap[String,Slot[SymbolMetadata]] ++ fieldMetadata

    def fields = children.keys
    def child(field: String) = children(field)

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
  }

  /**
   * Metadata for DeliteMultiArray
  */
  case class MultiArrayProperties (
    child: Slot[SymbolMetadata] = Unknown,
    __info: Iterable[(String, Slot[Metadata])] = Nil
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
  }


  // rank: Int,
  // var layout: Slot[Layout[_,_]] = Unknown,   // multiarray's data layout, Unknown to start
  // var buffer: Slot[Boolean] = Unknown        // multiarray's buffer impl, Unknown to start

  /** 
   * Shortcuts for comparing Slot[Metadata] and Slot[SymbolProperties]
   *
   * Would be really nice to reduce code duplication here with a type class for
   * equivalent, compatible, and meet operations, but Metadata is an abstract
   * class. meet() function returns type T (covariant) but getting an implementation of the
   * type class requires contravariance..
  */
  final def matches(a: Slot[Metadata], b: Slot[Metadata]): Boolean = (a,b) match {
    case (Known(am), Known(bm)) => am equivalent bm
    case (Unknown, Unknown) => true
    case _ => false
  }  
  final def canMeet(a: Slot[Metadata], b: Slot[Metadata]): Boolean = (a,b) match {
    case (Known(am), Known(bm)) => am compatible bm
    case (Unknown, Known(_)) => true
    case (Known(_), Unknown) => true
    case (Unknown, Unknown) => true
  }
  //TODO: throwing an exception here is a bit ugly - better way to do this?
  final def meet(a: Slot[Metadata], b: Slot[Metadata]): Slot[Metadata] = (a,b) match {
    case (Known(am), Known(bm)) if compatible(am,bm) => Known(am meet bm)
    case (Unknown, Known(bm)) => Known(bm)
    case (Known(am), Unknown) => Known(am)
    case (Unknown, Unknown) => Unknown
    case _ => throw new IllegalMeetException   // Shouldn't happen if compatible() is used
  }

  final def matches(a: Slot[SymbolProperties], b: Slot[SymbolProperties]): Boolean = (a,b) match {
    case (Known(am), Known(bm)) => am equivalent bm
    case (Unknown, Unknown) => true
    case _ => false
  }  
  final def canMeet(a: Slot[SymbolProperties], b: Slot[SymbolProperties]): Boolean = (a,b) match {
    case (Known(am), Known(bm)) => am compatible bm
    case (Unknown, Known(_)) => true
    case (Known(_), Unknown) => true
    case (Unknown, Unknown) => true
  }
  //TODO: throwing an exception here is a bit ugly - better way to do this?
  final def meet(a: Slot[SymbolProperties], b: Slot[SymbolProperties]): Slot[SymbolProperties] = (a,b) match {
    case (Known(am), Known(bm)) if compatible(am,bm) => Known(am meet bm)
    case (Unknown, Known(bm)) => Known(bm)
    case (Known(am), Unknown) => Known(am)
    case (Unknown, Unknown) => Unknown
    case _ => throw new IllegalMeetException   // Shouldn't happen if compatible() is used
  }

}