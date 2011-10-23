package ppl.dsl.optiql.baseline.util

import scala.reflect.NameTransformer._

object ReflectionHelper {
  implicit def any2reflection(x: Any) = new ReflectionHelper(x)
}

class ReflectionHelper(x:Any) {

  def methods = wrapped.getClass
      .getDeclaredMethods


  def methodsAsStrings = methods.toList.map(m => decode(m.toString
                        .replaceFirst("\\).*", ")")
                        .replaceAll("[^(]+\\.", "")
                        .replace("()", ""))).filter(!_.startsWith("$tag"))

  def fields = {
    val res = wrapped.getClass
      .getDeclaredFields
    res.foreach(f=> f.setAccessible(true))
    res
  }


   def fieldsAsStrings = fields.toList.map(m => decode(m.toString.replaceFirst("^.*\\.", "")))



  private def wrapped: AnyRef = x match {
    case x: Byte => byte2Byte(x)
    case x: Short => short2Short(x)
    case x: Char => char2Character(x)
    case x: Int => int2Integer(x)
    case x: Long => long2Long(x)
    case x: Float => float2Float(x)
    case x: Double => double2Double(x)
    case x: Boolean => boolean2Boolean(x)
    case _ => x.asInstanceOf[AnyRef]
  }
}