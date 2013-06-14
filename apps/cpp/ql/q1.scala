import java.io._
import scala.collection.mutable.ArrayBuffer

case class ArrayQ1(length: Int, l_quantity: Array[Double], l_extendedprice: Array[Double], l_discount: Array[Double], l_tax: Array[Double], l_returnflag: Array[Char], l_linestatus: Array[Char], l_shipdate: Array[Int])
case class ArrayResult(length: Int, returnFlag: Array[Char], lineStatus: Array[Char], sumQty: Array[Double], sumBasePrice: Array[Double], sumDiscountedPrice: Array[Double], sumCharge: Array[Double], avgQty: Array[Double], avgPrice: Array[Double], avgDiscount: Array[Double], countOrder: Array[Int])
case class TableResult(var data: TableArray, var size: Int)
case class TableArray(var returnFlag: Array[Char], var lineStatus: Array[Char], var sumQty: Array[Double], var sumBasePrice: Array[Double], var sumDiscountedPrice: Array[Double], var sumCharge: Array[Double], var avgQty: Array[Double], var avgPrice: Array[Double], var avgDiscount: Array[Double], var countOrder: Array[Int])

object Query1 {

  System.load(System.getProperty("user.dir") + "/q1.so")

  def main(args: Array[String]) {
    val numRuns = if (args.length > 1) args(1).toInt else 1
    for (i <- 0 until numRuns) {
      val path = args(0) + "/lineitem.tbl"
      val lineItems = loadLineItems(path)
      val date = parseDate("1998-12-01")
      println("querying " + lineItems.length + " records")
      val result = query1(lineItems, date)
      Table.printAsTable(TableResult(TableArray(result.returnFlag,result.lineStatus,result.sumQty,result.sumBasePrice,result.sumDiscountedPrice,result.sumCharge,result.avgQty,result.avgPrice,result.avgDiscount,result.countOrder), result.length))
    }
  }

  def loadLineItems(path: String) = {
    val l_quantity = new ArrayBuffer[Double]
    val l_extendedprice = new ArrayBuffer[Double]
    val l_discount = new ArrayBuffer[Double]
    val l_tax = new ArrayBuffer[Double]
    val l_returnflag = new ArrayBuffer[Char]
    val l_linestatus = new ArrayBuffer[Char]
    val l_shipdate = new ArrayBuffer[Int]

    val reader = new BufferedReader(new FileReader(path))
    var line = reader.readLine()
    //for (field <- line.split('|')) println(field)
    var i = 0
    while (line != null) {
      val fields = line.split('|')
      l_quantity += fields(4).toDouble
      l_extendedprice += fields(5).toDouble
      l_discount += fields(6).toDouble
      l_tax += fields(7).toDouble
      l_returnflag += fields(8)(0)
      l_linestatus += fields(9)(0)
      l_shipdate += parseDate(fields(10))

      line = reader.readLine()

      i += 1
      if (i%1000000 == 0) println("processed " + i/1000000 + " million records")
    }
    ArrayQ1(l_quantity.size, l_quantity.toArray, l_extendedprice.toArray, l_discount.toArray, l_tax.toArray, l_returnflag.toArray, l_linestatus.toArray, l_shipdate.toArray)
  }

  def parseDate(str: String) = {
    val tokens = str.split("-")
    val year = tokens(0).toInt //0 - 9999
    val month = tokens(1).toInt //1 - 12 (4 bits)
    val day = tokens(2).toInt //1 - 31 (5 bits)
    (year << 9) + (month << 5) + day
  }

  @native def query1(lineItems: ArrayQ1, date: Int): ArrayResult

}
