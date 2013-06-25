package ppl.dsl.optiql

import scala.virtualization.lms.common.Record
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._

trait Types { this: OptiQL =>
 
  trait Table[T] extends DeliteCollection[T]

  trait Grouping[K,V] extends DeliteCollection[V]

  trait Date

  type Result = Record

}
