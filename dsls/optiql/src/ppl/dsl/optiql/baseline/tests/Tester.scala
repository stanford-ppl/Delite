package ppl.dsl.optiql.baseline.tests

/*
import collection.mutable.ArrayBuffer

import QueryUtils._

object Tester {

  def main(args : Array[String]) : Unit = {

    val orders = new QVector[Order]
    orders += new Order(0, 1, 4, 10)
    orders += new Order(1, 1, 2, 7)
    orders += new Order(2, 1, 12, 250)
    orders += new Order(3, 3, 5, 200)
    orders += new Order(4, 3, 15, 6)
    orders += new Order(5, 4, 0, 44)
    orders += new Order(6, 6, 7, 7)
    orders += new Order(7, 6, 9, 20)
    orders += new Order(8, 6, 13, 3)
    orders += new Order(9, 6, 10, 12)
    orders += new Order(10, 8, 8, 40)
    orders += new Order(11, 9, 11, 33)
    orders += new Order(12, 11, 6, 20)
    orders += new Order(13, 11, 1, 4)
    orders += new Order(14, 12, 14, 2)
    orders += new Order(15, 12, 3, 12)

    val o1 = new QVector[Order]
    o1 += orders(0); o1 += orders(1); o1 += orders(2)
    val o2 = new QVector[Order]
    o2 += orders(3); o2 += orders(4)
    val o3 = new QVector[Order]
    o3 += orders(5)
    val o4 = new QVector[Order]
    o4 += orders(6); o4 += orders(7); o4 += orders(8); o4 += orders(9)
    val o5 = new QVector[Order]
    o5 += orders(10)
    val o6 = new QVector[Order]
    o6 += orders(11)
    val o7 = new QVector[Order]
    o7 += orders(12); o7 += orders(13)
    val o8 = new QVector[Order]
    o8 += orders(14); o8 += orders(15)

    val customers = new QVector[Customer]
    customers += new Customer(0, "Lere", 650468, new QVector[Order])
    customers += new Customer(1, "Nathan", 725000, o1)
    customers += new Customer(2, "Hassan", 948000, new QVector[Order])
    customers += new Customer(3, "Kunle", 650000, o2)
    customers += new Customer(4, "Darlene", 848000, o3)
    customers += new Customer(5, "Kunle", 234000, new QVector[Order])
    customers += new Customer(6, "Sang", 504000, o4)
    customers += new Customer(7, "Kunle", 212000, new QVector[Order])
    customers += new Customer(8, "Lawrence", 636000, o5)
    customers += new Customer(9, "Ervin", 754440, o6)
    customers += new Customer(10, "Kunle", 201000, new QVector[Order])
    customers += new Customer(11, "Lawrence", 988210, o7)
    customers += new Customer(12, "Sungpack", 989238, o8)
    customers += new Customer(13, "Lawrence", 495000, new QVector[Order])
    customers += new Customer(14, "Lawrence", 988210, new QVector[Order])
    customers += new Customer(15, "Xavier", 409222, new QVector[Order])

    val products = new QVector[Product]
    products += new Product(0, "Laptop", 1000.00, 10)
    products += new Product(1, "Sandwich", 5.00, 5)
    products += new Product(2, "DVD", 20.00, 50)
    products += new Product(3, "Table tennis paddle", 56.00, 2)
    products += new Product(4, "VGA cable", 10.00, 40)
    products += new Product(5, "Printer", 89.99, 8)
    products += new Product(6, "Cereal", 12.00, 20)
    products += new Product(7, "Novel", 8.90, 6)
    products += new Product(8, "Souvenir", 25.00, 200)
    products += new Product(9, "T-shirt", 18.00, 35)
    products += new Product(10, "Mortar and pestle", 60.00, 3)
    products += new Product(11, "Video game console", 380.00, 7)
    products += new Product(12, "Mouse", 3.50, 12)
    products += new Product(13, "Lined paper", 6.00, 8)
    products += new Product(14, "Plain paper", 7.00, 8)
    products += new Product(15, "Computer game", 39.99, 13)

    // SELECT TESTS
    val result1 = from c in customers select(c)
    result1.foreach(println)
    val result2 = from c in customers select(c.name)
    result2.foreach(println)
    val result3 = from c in customers select(c.getName())
    result3.foreach(println)
    val result4 = from c in customers select(c.getNameSubstring(3))
    result4.foreach(println)
    //val result5 = from c in customers select(c.getNameSubstring(1, 3)) //fails
    //result5.foreach(println)
    val result6 = from c in customers select( new {val name = c.name; val id = c.id} )
    result6.foreach((c) => println(c.name + ", " + c.id))

    // WHERE/SELECT TESTS
    val result7 = from c in customers where(c.id < 5) select(c.name)
    result7.foreach(println)
    val result8 = from c in customers where(c.name == "Lere") select(c)
    result8.foreach(println)
    val result9 = from c in customers where (c.getName() == "Hassan") select(c)
    result9.foreach(println)
    val result10 = from c in customers where(c.id < 10) select( new { val name = c.name ; val id = c.id})
    result10.foreach((c) => println(c.name + ", " + c.id))
    val result11 = from c in customers where (c.name.startsWith("Kun") && c.id < 7) select(c)
    result11.foreach(println)
    val result12 = from c in customers where (c.name.startsWith("Kun") && c.id > 3 && c.id < 7) select(c)
    result12.foreach(println)

    // ORDERING TESTS
    val result13 = from c in customers where(c.name == "Kunle") orderby(c.phone) select(c)
    result13.foreach(println)
    val result14 = from c in customers where(c.name == "Lawrence") orderby(c.phone desc) select(c)
    result14.foreach(println)
    val result15 = from c in customers where(c.name == "Lawrence") orderby(c.phone desc, c.id desc) select(c)
    result15.foreach(println)
    val result16 = from c in customers orderby(c.name, c.phone desc, c.id desc) select(c)
    result16.foreach(println)

    // GROUPING TESTS
    val result17 = from c in customers group(c by c.name)
    result17.foreach((x) => { println("Key:" + x._1); println("Values:"); x._2.foreach(println) })
    val result18 = from c in customers group(new {val id = c.id; val name = c.name} by c.name)
    result18.foreach((x) => { println("Key:" + x._1); println("Values:"); x._2.foreach((x) => println(x.id + ", " + x.name)) })

    // JOIN TESTS
    val result19 = from o in orders join (p ofType Product in products on o.prodID equals p.id) select (new {val name = p.name; val order = o.id}) //failure of type inference
    result19.foreach((x) => println("Name: " + x.name + "  Order: " + x.order))
    val result20 = from o in orders join (p ofType Product in products on o.prodID equals p.id into matching) select (new {val id = o.id; val count = matching.count})
    result20.foreach((x) => println("ID: " + x.id + "  Count: " + x.count))
    val result21 = from o in orders join(p ofType Product in products on o.prodID equals p.id) where(p.price > 10) select(p.name)
    result21.foreach(println)
    val result22 = from o in orders join(p ofType Product in products on o.prodID equals p.id) where(p.price > 10) join(c ofType Customer in customers on p.id equals c.id) select(c.name)
    result22.foreach(println)

    // CONTINUATION TESTS
    val result23 = from c in customers select(c) into again select(again.name)
    result23.foreach(println)
    val result24 = from c in customers where(c.name == "Lawrence") select(c) into Lawrence where(Lawrence.id == 14) select(Lawrence)
    result24.foreach(println)
    val result25 = from c in customers where(c.name == "Lawrence") select(c) into Lawrence where(Lawrence.id == 14) select(c) into single select(single.id)
    result25.foreach(println)

    // MULTIPLE GENERATOR TESTS
    /*val result26 = from c in customers where(c.name.length == 4) from o in c.orders where(o.qty > 10) select(new {val name = c.name; val qty = o.qty})
    result26.foreach((x) => println(x.one + ": " + x.two))*/

    /*val bogus = lkdsjflk
    println(bogus)*/
  }

}
*/