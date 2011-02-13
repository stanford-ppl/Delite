package ppl.dsl.optiql.baseline.tests


/**
 * Author: Lere Williams
 * Date: July 2009
 *
 * Description:
 */

class Order(val id: Int, val custID: Int, val prodID: Int, val qty: Int) {
  override def toString() = "OrderID:" + id + ", CustID:" + custID + ", ProdID:" + prodID + ", Qty:" + qty
}