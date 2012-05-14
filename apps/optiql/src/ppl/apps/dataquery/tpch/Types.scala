package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.OptiQLApplication

trait Types { this: OptiQLApplication =>

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
               shipInstructions: Rep[String], shipMode: Rep[String], comment: Rep[String]): Rep[LineItem] = new Record {
    val l_orderkey = orderKey
    val l_partkey = partKey
    val l_suppkey = supplierKey
    val l_linenumber = lineNumber
    val l_quantity = quantity
    val l_extendedprice = extendedPrice
    val l_discount = discount
    val l_tax = tax
    val l_returnflag = returnFlag
    val l_linestatus = lineStatus
    val l_shipdate = shipDate
    val l_commitdate = commitDate
    val l_receiptdate = receiptDate
    val l_shipinstruct = shipInstructions
    val l_shipmode = shipMode
    val l_comment = comment
  }

  def LineItem(): Rep[LineItem] = LineItem(0, 0, 0, 0, 0, 0, 0, 0, unit('\0'), unit('\0'), Date(""), Date(""), Date(""), "", "", "")

  type LineItemTable = DataTable[LineItem]

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
               acctBal: Rep[Double], marketSegment: Rep[String], comment: Rep[String]): Rep[Customer] = new Record {
    val c_custkey = key
    val c_name = name
    val c_address = address
    val c_nationkey = nationKey
    val c_phone = phone
    val c_acctbal = acctBal
    val c_mktsegment = marketSegment
    val c_comment = comment
  }

  def Customer(): Rep[Customer] = Customer(0, "", "", 0, "", 0, "", "") //TODO: no lift for Double?

  type CustomerTable = DataTable[Customer]

  type Nation = Record {
    val n_nationkey: Int
    val n_name: String
    val n_regionkey: Int
    val n_comment: String
  }

  def Nation(key: Rep[Int], name: Rep[String], regionKey: Rep[Int], comment: Rep[String]): Rep[Nation] = new Record {
    val n_nationkey = key
    val n_name = name
    val n_regionkey = regionKey
    val n_comment = comment
  }

  def Nation(): Rep[Nation] = Nation(0, "", 0, "")

  type NationTable = DataTable[Nation]

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
            comment: Rep[String]): Rep[Order] = new Record {
    val o_orderkey = key
    val o_custkey = customerKey
    val o_orderstatus = orderStatus
    val o_totalprice = totalPrice
    val o_orderdate = orderDate
    val o_orderpriority = orderPriority
    val o_clerk = clerk
    val o_shippriority = shipPriority
    val o_comment = comment
  }

  def Order(): Rep[Order] = Order(0, 0, unit('\0'), 0, Date(""), "", "", 0, "")

  type OrderTable = DataTable[Order]

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
           size: Rep[Int], container: Rep[String], retailPrice: Rep[Double], comment: Rep[String]): Rep[Part] = new Record {
    val p_partkey = key
    val p_name = name
    val p_mfgr = manufacturer
    val p_brand = brand
    val p_type = partType
    val p_size = size
    val p_container = container
    val p_retailprice = retailPrice
    val p_comment = comment
  }

  def Part(): Rep[Part] = Part(0, "", "", "", "", 0, "", 0, "")

  type PartTable = DataTable[Part]

  type PartSupplier = Record {
    val ps_partkey: Int
    val ps_suppkey: Int
    val ps_availqty: Int
    val ps_supplycost: Double
    val ps_comment: String
  }

  def PartSupplier(partKey: Rep[Int], supplierKey: Rep[Int], availableQty: Rep[Int],
                   supplyCost: Rep[Double], comment: Rep[String]): Rep[PartSupplier] = new Record {
    val ps_partkey = partKey
    val ps_suppkey = supplierKey
    val ps_availqty = availableQty
    val ps_supplycost = supplyCost
    val ps_comment = comment
  }

  def PartSupplier(): Rep[PartSupplier] = PartSupplier(unit(0), unit(0), unit(0), unit(0.0), unit(""))

  type PartSupplierTable = DataTable[PartSupplier]

  type Region = Record {
    val r_regionkey: Int
    val r_name: String
    val r_comment: String
  }

  def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]): Rep[Region] = new Record {
    val r_regionkey = key
    val r_name = name
    val r_comment = comment
  }

  def Region(): Rep[Region] = Region(0,"","")

  type RegionTable = DataTable[Region]

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
               phone: Rep[String], acctBal: Rep[Double], comment: Rep[String]): Rep[Supplier] = new Record {
    val s_suppkey = key
    val s_name = name
    val s_address = address
    val s_nationkey = nationKey
    val s_phone = phone
    val s_acctbal = acctBal
    val s_comment = comment
  }

  def Supplier(): Rep[Supplier] = Supplier(0, "", "", 0, "", 0, "")

  type SupplierTable = DataTable[Supplier]

}
