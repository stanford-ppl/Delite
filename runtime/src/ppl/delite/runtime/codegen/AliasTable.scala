package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer

final class AliasTable[T] {

  private val table = new ArrayBuffer[Set[T]]

  private[codegen] def get(name: T): Set[T] = {
    val sets = table.filter(s => s.contains(name))
    assert(sets.length < 2)
    if (sets.length == 0) Set(name)
    else sets(0)
  }

  private[codegen] def add(name1: T, name2: T) {
    val sets = table.filter(s => s.contains(name1) || s.contains(name2))
    assert(sets.length < 3)
    if (sets.length == 0) {
      table += Set(name1,name2)
    }
    else {
      val newSet = sets.reduceLeft(_ union _)
      for (s <- sets) table -= s
      table += newSet
    }
  }

  private[codegen] def remove(name: T) {
    val sets = table.filter(s => s.contains(name))
    assert(sets.length == 1)
    table -= sets(0)
    table += (sets(0)-name)
  }

  private[codegen] def clear {
    table.clear
  }

}