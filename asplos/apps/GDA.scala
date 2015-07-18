import asplos._

object GDA extends PPLCompiler {
  def main() = {
    val x = read2D("/home/david/PPL/data/1024-1200x.dat")
    val y = read("/home/david/PPL/data/q1y.dat").map{e => e.toDouble > 0.0}

    val m = y.length  // Number of training samples 
    val n = x.nCols   // Dimensionality of training data
    println("n = " + n)
    println("m = " + m)

    val y_zeros = filterReduce(m, 0){  y(_) }{r => 1 }{_+_}
    val y_ones  = filterReduce(m, 0){ !y(_) }{r => 1 }{_+_}

    val zeroVec0 = collect(n){i => 0.0}.asView
    val zeroVec1 = collect(n){i => 0.0}.asView
    val mu0_num = filterReduce(m, zeroVec0){  y(_) }{r => x.slice(r, *) }{(a,b) => collect(n){v => a(v) + b(v) }.asView }.notePhysViewOnly
    val mu1_num = filterReduce(m, zeroVec1){ !y(_) }{r => x.slice(r, *) }{(a,b) => collect(n){v => a(v) + b(v) }.asView }.notePhysViewOnly

    val phi = 1.0 / m * y_ones
    val mu0 = collect(n){v => mu0_num(v) / y_zeros }
    val mu1 = collect(n){v => mu1_num(v) / y_ones }

    val sigma = reduce(m)( Array2D[Double](n,n) ){r => 
      val mu = if (y(r)) mu1 else mu0
      val row = x.slice(r, *)
      val sub = collect(n){i => row(i) - mu(i)}
      collect(n,n){(i,j) => sub(i) * sub(j) }
    }{(a,b) => 
      collect(n,n){(i,j) => a(i,j) + b(i,j)}
      //forIndices(n,n){(i,j) => a(i,j) = a(i,j) + b(i,j) }
      //(a)
    }

    println("phi = " + phi)
    println("mu0 = "); mu0.slice(0 :@: 4).pprint
    println("mu1 = "); mu1.slice(0 :@: 4).pprint
    sigma.slice(0 :@: 4, 0 :@: 4).pprint
  }
}

/*
object GDABlockedLiteCompiler extends PPLCompiler with GDABlockedLite
object GDAFunctionLite extends PPLCompiler with GDABlockedLite {
  registerFunction(gdaSigma _)
  override def functionName = "GDA"
}

trait GDABlockedLite extends PPLApp {
  def gdaSigma(xData: Rep[Array1D[Double]], y: Rep[Array1D[Double]], mu0: Rep[Array1D[Double]], mu1: Rep[Array1D[Double]], m: Rep[Int], n: Rep[Int]): Rep[Array2D[Double]] = {
    val x = Array2D(xData, m, n)
    val zeroMat = Array2D[Double](n,n)
    
    reduce(m, zeroMat, mutable = true){r => 
      val mu = if (y(r)) mu1 else mu0
      val row = x.slice(r, *)
      val sub = collect(n){i => row(i) - mu(i)}
      collect(n,n){(i,j) => sub(i) * sub(j) }
    }{(a,b) => 
      forIndices(n,n){(i,j) => a(i,j) = b(i,j) }
      (a)
    }
  }

  def main() = {
    val x = read2D("/home/david/PPL/data/1024-1200x.dat")
    val y = read("/home/david/PPL/data/q1y.dat").map{e => e.toDouble <= 0.0}

    val m = y.length  // Number of training samples 
    val n = x.nCols   // Dimensionality of training data
    println("n = " + n)
    println("m = " + m)

    val y_zeros = filterReduce(m, 0){ !y(_) }{r => 1 }{_+_}
    val y_ones  = filterReduce(m, 0){  y(_) }{r => 1 }{_+_}

    val zeroVec0 = collect(n){i => 0.0}.asView
    val zeroVec1 = collect(n){i => 0.0}.asView
    val mu0_num = filterReduce(m, zeroVec0){ !y(_) }{r => x.slice(r, *) }{(a,b) => collect(n){v => a(v) + b(v) }.asView }.notePhysViewOnly
    val mu1_num = filterReduce(m, zeroVec1){  y(_) }{r => x.slice(r, *) }{(a,b) => collect(n){v => a(v) + b(v) }.asView }.notePhysViewOnly

    val phi = 1.0 / m * y_ones
    val mu0 = collect(n){v => mu0_num(v) / y_zeros }
    val mu1 = collect(n){v => mu1_num(v) / y_ones }

    val sigma = gdaSigma(x.data, y, mu0, mu1, m, n)

    println("phi = " + phi)
    println("mu0 = "); mu0.pprint
    println("mu1 = "); mu1.pprint
    println(sigma(0,0))
  }
}*/
