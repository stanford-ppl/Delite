package ppl.dsl.optiml.baseline

object LanguageDefs {
  val INITIAL_SEED = 100
  var randRef = new scala.util.Random(INITIAL_SEED)
  var _time = 0L
  
  def random(): Double = randRef.nextDouble
    
  def abs(d: Double) = implicitly[Numeric[Double]].abs(d) //java.lang.Math.abs(d)
  
  def sum[B:Numeric:Manifest](st: Int, en: Int)(f: Int => B) = {
    var acc = f(st)
    for (i <- st+1 until en) {
      acc = implicitly[Numeric[B]].plus(acc,f(i))
    }
    acc
  }
  
  def tic() {
    _time = System.currentTimeMillis()
  }
  
  def toc() {
    val now = System.currentTimeMillis()
    println("elapsed: " + (now-_time)/1000.0)
  }
}