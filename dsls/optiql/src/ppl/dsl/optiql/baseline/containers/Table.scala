package ppl.dsl.optiql.baseline.containers

import collection.mutable.{ArrayBuffer, BufferLike}
import collection.generic.CanBuildFrom
import collection.Iterable
import ppl.dsl.optiql.baseline.util.{ReflectionHelper, Date}
import ppl.dsl.optiql.baseline.OptiQL

//this includes functionality for loading TPCH style data
abstract class Table[TSource] extends Iterable[TSource] {

  val data = new ArrayBuffer[TSource]

  val grouped = false

  def iterator = data.iterator

  override def filter(p: TSource => Boolean ) = {

    val ndata = data.filter(p)
    new Table[TSource] {

      override val data = ndata

      def addRecord(arr: Array[String]) {
        throw new RuntimeException("Cannot  add Record into a projected Table")
      }
    }

  }


  override def map[B, That](f: (TSource) => B)(implicit bf: CanBuildFrom[Iterable[TSource], B, That]) = {
    data.map(f)
  }

  def addRecord(fields: Array[String])

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
      val table = OptiQL.convertIterableToTable(group.elems)
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
}