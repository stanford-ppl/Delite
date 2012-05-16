package ppl.dsl.optiql

import ppl.delite.framework.ops.DeliteCollection

trait Types { this: OptiQL =>

  abstract class DataTable[T] extends Record with DeliteCollection[T]

  abstract class Grouping[K,V] extends Record with DeliteCollection[V]

  abstract class Date

}
