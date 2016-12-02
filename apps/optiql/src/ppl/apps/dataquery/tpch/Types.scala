package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.OptiQLApplication
import scala.virtualization.lms.common.Record
import scala.virtualization.lms.common.RecordOps

trait Types extends RecordOps { this: OptiQLApplication =>

  type LineItem = Record {
    val l_orderkey: Int
    val l_partkey: Int
    val l_suppkey: Int
    val l_linenumber: Int
    val l_quantity: Double
    val l_extendedprice: Double
    val l_discount: Double
    val l_tax: Double
    val l_returnflag: Char
    val l_linestatus: Char
    val l_shipdate: Date
    val l_commitdate: Date
    val l_receiptdate: Date
    val l_shipinstruct: String
    val l_shipmode: String
    val l_comment: String
  }

  def LineItem(orderKey: Rep[Int], partKey: Rep[Int], supplierKey: Rep[Int], lineNumber: Rep[Int], quantity: Rep[Double],
               extendedPrice: Rep[Double], discount: Rep[Double], tax: Rep[Double], returnFlag: Rep[Char],
               lineStatus: Rep[Char], shipDate: Rep[Date], commitDate: Rep[Date], receiptDate: Rep[Date],
               shipInstructions: Rep[String], shipMode: Rep[String], comment: Rep[String]): Rep[LineItem] = Record (
    l_orderkey = orderKey,
    l_partkey = partKey,
    l_suppkey = supplierKey,
    l_linenumber = lineNumber,
    l_quantity = quantity,
    l_extendedprice = extendedPrice,
    l_discount = discount,
    l_tax = tax,
    l_returnflag = returnFlag,
    l_linestatus = lineStatus,
    l_shipdate = shipDate,
    l_commitdate = commitDate,
    l_receiptdate = receiptDate,
    l_shipinstruct = shipInstructions,
    l_shipmode = shipMode,
    l_comment = comment
  )

  type Customer = Record {
    val c_custkey: Int
    val c_name: String
    val c_address: String
    val c_nationkey: Int
    val c_phone: String
    val c_acctbal: Double
    val c_mktsegment: String
    val c_comment: String
  }

  def Customer(key: Rep[Int], name: Rep[String], address: Rep[String], nationKey: Rep[Int], phone: Rep[String],
               acctBal: Rep[Double], marketSegment: Rep[String], comment: Rep[String]): Rep[Customer] = Record (
    c_custkey = key,
    c_name = name,
    c_address = address,
    c_nationkey = nationKey,
    c_phone = phone,
    c_acctbal = acctBal,
    c_mktsegment = marketSegment,
    c_comment = comment
  )

  type Nation = Record {
    val n_nationkey: Int
    val n_name: String
    val n_regionkey: Int
    val n_comment: String
  }

  def Nation(key: Rep[Int], name: Rep[String], regionKey: Rep[Int], comment: Rep[String]): Rep[Nation] = Record (
    n_nationkey = key,
    n_name = name,
    n_regionkey = regionKey,
    n_comment = comment
  )

  type Order = Record {
    val o_orderkey: Int
    val o_custkey: Int
    val o_orderstatus: Char
    val o_totalprice: Double
    val o_orderdate: Date
    val o_orderpriority: String
    val o_clerk: String
    val o_shippriority: Int
    val o_comment: String
  }

  def Order(key: Rep[Int], customerKey: Rep[Int], orderStatus: Rep[Char], totalPrice: Rep[Double],
            orderDate: Rep[Date], orderPriority: Rep[String], clerk: Rep[String], shipPriority: Rep[Int],
            comment: Rep[String]): Rep[Order] = Record (
    o_orderkey = key,
    o_custkey = customerKey,
    o_orderstatus = orderStatus,
    o_totalprice = totalPrice,
    o_orderdate = orderDate,
    o_orderpriority = orderPriority,
    o_clerk = clerk,
    o_shippriority = shipPriority,
    o_comment = comment
  )

  type Part = Record {
    val p_partkey: Int
    val p_name: String
    val p_mfgr: String
    val p_brand: String
    val p_type: String
    val p_size: Int
    val p_container: String
    val p_retailprice: Double
    val p_comment: String
  }

  def Part(key: Rep[Int], name: Rep[String], manufacturer: Rep[String], brand: Rep[String], partType: Rep[String],
           size: Rep[Int], container: Rep[String], retailPrice: Rep[Double], comment: Rep[String]): Rep[Part] = Record (
    p_partkey = key,
    p_name = name,
    p_mfgr = manufacturer,
    p_brand = brand,
    p_type = partType,
    p_size = size,
    p_container = container,
    p_retailprice = retailPrice,
    p_comment = comment
  )

  type PartSupplier = Record {
    val ps_partkey: Int
    val ps_suppkey: Int
    val ps_availqty: Int
    val ps_supplycost: Double
    val ps_comment: String
  }

  def PartSupplier(partKey: Rep[Int], supplierKey: Rep[Int], availableQty: Rep[Int],
                   supplyCost: Rep[Double], comment: Rep[String]): Rep[PartSupplier] = Record (
    ps_partkey = partKey,
    ps_suppkey = supplierKey,
    ps_availqty = availableQty,
    ps_supplycost = supplyCost,
    ps_comment = comment
  )

  type Region = Record {
    val r_regionkey: Int
    val r_name: String
    val r_comment: String
  }

  def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]): Rep[Region] = Record (
    r_regionkey = key,
    r_name = name,
    r_comment = comment
  )

  type Supplier = Record {
    val s_suppkey: Int
    val s_name: String
    val s_address: String
    val s_nationkey: Int
    val s_phone: String
    val s_acctbal: Double
    val s_comment: String
  }

  def Supplier(key: Rep[Int], name: Rep[String], address: Rep[String], nationKey: Rep[Int],
               phone: Rep[String], acctBal: Rep[Double], comment: Rep[String]): Rep[Supplier] = Record (
    s_suppkey = key,
    s_name = name,
    s_address = address,
    s_nationkey = nationKey,
    s_phone = phone,
    s_acctbal = acctBal,
    s_comment = comment
  )

}
