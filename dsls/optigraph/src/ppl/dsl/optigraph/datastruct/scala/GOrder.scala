
package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{LinkedHashSet}

class GOrder[@specialized T: ClassManifest] {
  // Use a LinkedHashSet:
  // "This class implements mutable sets using a hashtable.
  // The iterator and all traversal methods of this class visit
  // elements in the order they were inserted."
  var _data: LinkedHashSet[T] = new LinkedHashSet[T]()
}