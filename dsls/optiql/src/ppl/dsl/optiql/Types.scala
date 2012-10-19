package ppl.dsl.optiql

import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._

trait Types { this: OptiQL =>
 
  abstract class Table[T] extends Record with DeliteCollection[T]

  abstract class Grouping[K,V] extends Record with DeliteCollection[V]

  abstract class Date

  type Result = Record

}
