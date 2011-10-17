package ppl.delite.benchmarking.sorting.tpch

import java.io._
import ppl.delite.benchmarking.sorting.Config
import scala.collection.mutable.ArrayBuffer
import ppl.delite.benchmarking.sorting.tpch.util.Date



object TPCH {

  val tpchDataPath= Config.tpch_dir + File.separator + "SF" + Config.tpch_factor
  val debug = true

  //implicit conversions
  implicit def cStrToInt(s: String) = Integer.parseInt(s)
  implicit def cStrToFloat(s: String) = java.lang.Float.parseFloat(s)
  implicit def cStrToDouble(s: String) = java.lang.Double.parseDouble(s)
  implicit def cStrToChar(s: String) = {
    assert(s.size == 1, "expected char, got: " + s)
    s(0)
  }
  implicit def cStrToDate(s: String) = Date(s)

  //pretty horrendous hack to use an int to decide which collection to load, but this is what you get with loss of having all these tables
  //should be using builders
  def loadTPCHTable[T:Manifest](collectionType: Int, maxItems: Int = 0) =  {
      val path = collectionType match {
        case 1 => "lineitem"
        case _ => sys.error("Unsupported collection type")
      }
      log("loading tpch table from file[" + tpchDataPath + File.separator + path +"] into memory")
      val filename = tpchDataPath + File.separator + path + ".tbl"
      val file = new File(filename)
      if(file.isFile == false) throw new RuntimeException(filename + " doesn't appear to be a valid file")
      //load each line
      val records = scala.io.Source.fromFile(file).getLines()
      //estimate number of entries based on load factor
      val init_size = collectionType match {
        case 1 => Config.tpch_factor*7000000 + 100 // the 100 is to handle SF0
        case _ => 100
      }
      val data = new ArrayBuffer[T](init_size) {
        def getArray = array
      }
      var i = 0
      var finish = false
      while(!finish && records.hasNext) {
        val record = records.next
        val fs = record.split('|')
        //dispatch based on type
        collectionType match {
          case 1 => data.append((new LineItem(fs(0),fs(1),fs(2),fs(3),fs(4),fs(5),fs(6),fs(7),fs(8),fs(9),fs(10),fs(11),fs(12),fs(13),fs(14),fs(15))).asInstanceOf[T]) 
          case _ => sys.error("Unsupported collection type") //only support lineitems for now
        }                        
        i += 1
        if(i%500000 == 0) {
          println("processed " + i + " records")
          if(maxItems !=0 && i >= maxItems)
            finish = true
        }
      }
      data
  }

  def log(msg: String) {
    if(debug) println(msg)
  }
  
  //constants
  val CUSTOMERS = 0
  val LINEITEMS = 1
  val NATION = 2
  val ORDERS = 3
  val PARTS = 4
  val PARTSUPPLIERS = 5
  val REGIONS = 6
  val SUPPLIERS = 7


}
