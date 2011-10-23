package ppl.dsl.optiql.baseline.tests


class Product(val id: Int, val name: String, val price: Double, val stock: Int) {
  override def toString() = "Product:" + name + ", Price:" + price
}