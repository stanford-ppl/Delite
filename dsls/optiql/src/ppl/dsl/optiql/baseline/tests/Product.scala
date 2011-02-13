package ppl.dsl.optiql.baseline.tests


/**
 * Author: Lere Williams
 * Date: July 2009
 *
 * Description:
 */

class Product(val id: Int, val name: String, val price: Double, val stock: Int) {
  override def toString() = "Product:" + name + ", Price:" + price
}