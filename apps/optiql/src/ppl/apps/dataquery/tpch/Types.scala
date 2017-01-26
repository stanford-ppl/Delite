package ppl.apps.dataquery.tpch

import ppl.dsl.optiql.OptiQLApplication
import org.scala_lang.virtualized._

trait Types extends RecordOps { this: OptiQLApplication =>

  @mRecord
  case class LineItem(
    l_orderkey: Int,
    l_partkey: Int,
    l_suppkey: Int,
    l_linenumber: Int,
    l_quantity: Double,
    l_extendedprice: Double,
    l_discount: Double,
    l_tax: Double,
    l_returnflag: Char,
    l_linestatus: Char,
    l_shipdate: Date,
    l_commitdate: Date,
    l_receiptdate: Date,
    l_shipinstruct: String,
    l_shipmode: String,
    l_comment: String
  )

  @mRecord
  case class Customer(
    c_custkey: Int,
    c_name: String,
    c_address: String,
    c_nationkey: Int,
    c_phone: String,
    c_acctbal: Double,
    c_mktsegment: String,
    c_comment: String
  )

  @mRecord
  case class Nation (
    n_nationkey: Int,
    n_name: String,
    n_regionkey: Int,
    n_comment: String
  )

  @mRecord
  case class Order(
    o_orderkey: Int,
    o_custkey: Int,
    o_orderstatus: Char,
    o_totalprice: Double,
    o_orderdate: Date,
    o_orderpriority: String,
    o_clerk: String,
    o_shippriority: Int,
    o_comment: String
  )

  @mRecord
  case class Part(
    p_partkey: Int,
    p_name: String,
    p_mfgr: String,
    p_brand: String,
    p_type: String,
    p_size: Int,
    p_container: String,
    p_retailprice: Double,
    p_comment: String
  )

  @mRecord
  case class PartSupplier(
    ps_partkey: Int,
    ps_suppkey: Int,
    ps_availqty: Int,
    ps_supplycost: Double,
    ps_comment: String
  )

  @mRecord
  case class Region(
    r_regionkey: Int,
    r_name: String,
    r_comment: String
  )

  @mRecord
  case class Supplier(
    s_suppkey: Int,
    s_name: String,
    s_address: String,
    s_nationkey: Int,
    s_phone: String,
    s_acctbal: Double,
    s_comment: String
  )
  
}
