package ppl.dsl.optiql.datastruct.scala.container

import collection.mutable.ArrayBuffer
import collection.mutable.{ArrayBuffer, HashMap, Buffer}
import ppl.dsl.optiql.datastruct.scala.util.{ReflectionHelper, Date}

object DataTable {

  implicit def convertIterableToDataTable[T](i: Iterable[T]) : DataTable[T] = {
    if(i.isInstanceOf[DataTable[T]]) {
      i.asInstanceOf[DataTable[T]]
    }
    else if(i.isInstanceOf[ArrayBuffer[T]]) {

      return new DataTable[T] {

        data = i.asInstanceOf[ArrayBuffer[T]]

        override def addRecord(arr: Array[String]) {
          throw new RuntimeException("Cannot add Record into a projected DataTable")
        }
      }

    }
	else if(i.isInstanceOf[Grouping[_,T]]) {
	  println("converting grouping to DataTable")
	  val g = i.asInstanceOf[Grouping[_,T]]
	  assert(g.elems.isInstanceOf[ArrayBuffer[T]])
	  return new DataTable[T] {

        data = g.elems.asInstanceOf[ArrayBuffer[T]]

        override def addRecord(arr: Array[String]) {
          throw new RuntimeException("Cannot add Record into a projected DataTable")
        }
      }
	  
	}
    else {
      val arrbuf = new ArrayBuffer[T]();
      for (e <- i) {
        arrbuf.append(e)
      }
      return new DataTable[T] {

        data = arrbuf

        override def addRecord(arr: Array[String]) {
          throw new RuntimeException("Cannot add Record into a projected DataTable")
        }
      }
	}
  }
}

class DataTable[TSource](initialSize: Int) extends Iterable[TSource] with ppl.delite.framework.datastruct.scala.DeliteCollection[TSource] {
  import DataTable._

  println("initialSize : " + initialSize)
  var data = ArrayBuffer.fill[TSource](initialSize)(null.asInstanceOf[TSource])
  println("size of internal data is : " + data.size)
  val grouped = false
  def iterator = data.iterator


  def dcApply(idx: Int) = data(idx)
  def dcUpdate(idx: Int, x: TSource) {
    data(idx) = x
  }
  
  def this() = this(0)
	

  //TODO HCXXX: NEED TO REMOVE ALL THIS STUFF OR ADD IT TO DELITE COLLECTION
  def apply(idx: Int): TSource = data(idx)
  def cloneL = new DataTable[TSource]
  def insert(pos: Int, x: TSource) {
    data.insert(pos, x)
  }
  def insertAll(pos: Int, x: DataTable[TSource]) {
    data.insertAll(pos, x)
  }
  def length = data.size


  def addRecord(fields: Array[String]): Unit = throw new RuntimeException("You forgot to implement addRecord for " + this.getClass)

  implicit def cStrToInt(s: String) = Integer.parseInt(s)
  implicit def cStrToFloat(s: String) = java.lang.Float.parseFloat(s)
  implicit def cStrToChar(s: String) = {
    assert(s.size == 1, "expected char, got: " + s)
    s(0)
  }
  implicit def cStrToDate(s: String) = Date(s)


    def repeat(s: String, n:Int)(implicit sb: StringBuilder) {
    //assert(n < 0 || n > 300, "Incorrect value supplied for n in repeat")
    //todo for now, just ignore bad value of n
    if(n < 0 || n > 300)
      return
    var idx = 0
    while(idx != n) {
      sb.append(s);
      idx += 1
    }
  }

  //todo clean this up
  //for example, I can use reflection to generate the metadata that I need to print the table
  def printAsTable() {

    // Check if Table is empty
    if(data.size == 0) {
      println("=====================================================")
      println("|                  EMPTY TABLE                      |")
      println("=====================================================")
      return
    }

    // Check if we are dealing with a grouping

    if(grouped) {
      handleGroupedTable
    } else {
      handleNormalTable
    }

  }

  private def handleGroupedTable() {
    for(key <- data) {
      val group = key.asInstanceOf[Grouping[_,_]]
      println("Key = " + group.key)
      val table = convertIterableToDataTable(group.elems)
      table.printAsTable
    }

  }

  private def handleNormalTable() {
    implicit val tableStr = new StringBuilder
    val columnSizes = getTableColSizes()

    def horizontalRule = {
      for(i <- 0 until columnSizes.size )
        repeat("=",columnSizes(i)+1)
      tableStr append("=\n")
    }
    def newLine = {
      tableStr append("\n")
    }

    horizontalRule
    import ReflectionHelper._
    val fieldStrs = data.head.fieldsAsStrings
    tableStr append("|")
    for(i <- 0 until columnSizes.size) {
      tableStr append( " " + fieldStrs(i))
      repeat(" " , columnSizes(i) - fieldStrs(i).size - 1  )
      tableStr append("|")
    }
    newLine
    horizontalRule
    print(tableStr.toString)
    tableStr.clear

    for(r <- 0 until data.size) {
      emitRecordAsRow(r, columnSizes)
    }
    print(tableStr.toString)
    tableStr.clear

    horizontalRule
    println(tableStr.toString)
  }



  private def max(a: Int, b: Int) = if(a > b)  a else b

  private def getTableColSizes(): Array[Int] = {
    if(data.size==0)
      return new Array[Int](0)

    val datum = data.head
    import ReflectionHelper._
    val fieldStrs = datum.fieldsAsStrings
    val fields = datum.fields

    val columnSizes = new Array[Int](fields.size)

    //Columns should be at least the size of the headers
    var idx = 0
    for(f <- fieldStrs) {
      columnSizes(idx) = max(columnSizes(idx), f.length + 2)
      idx += 1
    }

    //columns should be at least the size of maximal element
    for(d <- data) {
      idx = 0
      while ( idx < fields.size) {
        columnSizes(idx) = max(columnSizes(idx), fields(idx).get(d).toString.length + 2)
        idx += 1
      }
    }

    return columnSizes
  }

  def emitRecordAsRow(r: Int, columnSizes: Array[Int])(implicit sb:StringBuilder) {
    sb append("| ")
    var str = ""

    val row = data(r)

    import ReflectionHelper._
    val fields = row.fields

    var idx = 0
    for(field <- fields) {
      str = field.get(row).toString
      sb append(str); repeat(" ", columnSizes(idx) - str.size - 1); sb append("| ")
      idx += 1
    }
    sb append("\n")
  }

  def forbid = throw new RuntimeException("Should not be using this method, got here by mistake")
  def notImplemented = throw new RuntimeException("Not Implemented Yet")
  
  
  //This part is hacked so I can run optiql in sequential mode
  def GroupBy[TKey](keySelector: TSource => TKey) = {
    //println("constructing hash-table for grouping operation")
    val (hTable, keys) = buildHash(this,keySelector)
    val result = new DataTable[Grouping[TKey,TSource]] {
      override def addRecord(fields: Array[String]) = throw new RuntimeException("Cannot add records to a grouping table")
      override val grouped = true
    }
    for(key <- keys) {
      result.data += new Grouping(key,hTable.getOrElse(key, new ArrayBuffer[TSource]))
    }
    result
  }
  
  /*****
   * Internal Implementation functions
   */
  private def buildHash[TSource,TKey](source:Iterable[TSource], keySelector: TSource => TKey) = {
    val hash = HashMap[TKey, ArrayBuffer[TSource]]()
    val keys = new ArrayBuffer[TKey]
    for (elem <- source; key = keySelector(elem)) {
      hash.getOrElseUpdate(key,{
        keys.append(key)
        new ArrayBuffer[TSource]() //if there is no key
      }) += elem
    }
    (hash,keys)
  }

}