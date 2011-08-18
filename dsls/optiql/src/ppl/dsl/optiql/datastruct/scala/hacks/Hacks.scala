package ppl.dsl.optiql.datastruct.scala.hacks

//these represent hacked return type for TPCH due to lack to new constructor lifting
class Q1(val returnFlag: Char, 
         val lineStatus: Char, 
		 val sumQty: Double, 
         val sumBasePrice: Double, 
         val sumDiscountedPrice: Double, 
         val sumCharge: Double, 
         val avgQty: Double, 
         val avgPrice: Double, 
         val avgDiscount: Double, 
		 val count: Int)

