package ppl.dsl.optiql.datastruct.scala.hacks

//these represent hacked return type for TPCH due to lack to new constructor lifting
class Q1(val returnFlag: Char, 
         val lineStatus: Char, 
		 val sumQty: Float, 
         val sumBasePrice: Float, 
         val sumDiscountedPrice: Float, 
         val sumCharge: Float, 
         val avgQty: Float, 
         val avgPrice: Float, 
         val avgDiscount: Float, 
		 val count: Int)

