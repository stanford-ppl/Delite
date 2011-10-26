package ppl.dsl.optiql.baseline


/*

object QueryUtils {

  //Convenience for ordering methods (allows you to leave off ordering function)
  case class ComparisonFunction[A](val pred: (A, A) => Boolean)
  implicit val stringDefault:ComparisonFunction[String] = ComparisonFunction[String](_ < _)
  implicit val intDefault:ComparisonFunction[Int] = ComparisonFunction[Int](_ < _)
  implicit val doubleDefault:ComparisonFunction[Double] = ComparisonFunction[Double](_ < _)
  implicit val floatDefault:ComparisonFunction[Float] = ComparisonFunction[Float](_ < _)
  implicit val longDefault:ComparisonFunction[Long] = ComparisonFunction[Long](_ < _)
  implicit val shortDefault:ComparisonFunction[Short] = ComparisonFunction[Short](_ < _)
  implicit val charDefault:ComparisonFunction[Char] = ComparisonFunction[Char](_ < _)
  implicit val byteDefault:ComparisonFunction[Byte] = ComparisonFunction[Byte](_ < _)

  /*Convenience for QVector size exposure (allows you to reveal the allocated size of
    the QVector.  Useful but dangerous: could potentially retrieve unitialized elements.
    This is really only meant to be used internally*/
  case class ExposureFlag(val flag:Boolean)
  implicit val defaultExposure = ExposureFlag(false)

}
*/