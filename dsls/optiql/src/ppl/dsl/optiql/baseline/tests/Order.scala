package ppl.dsl.optiql.baseline.tests

class Order(val id: Int, val custID: Int, val prodID: Int, val qty: Int) {
  override def toString() = "OrderID:" + id + ", CustID:" + custID + ", ProdID:" + prodID + ", Qty:" + qty
}