import asplos._

object GDACompiler extends SMALCompiler with GDA
trait GDA extends SMALApp {
  def main() {
    val x = read2D("/home/david/PPL/data/1024-1200x.dat")
    val y = read1D("/home/david/PPL/data/q1y.dat").map{d => d > 0.0}

    tic()

    val m = y.length // Number of training samples
    val n = x.nCols  // Dimesionality of training data

    println("n = " + n)
    println("m = " + m)
    val y_zeros = y count { _ == false }
    val y_ones = y count { _ == true }
    val mu0_num = x.sumRowsIf{  !y(_) }
    val mu1_num = x.sumRowsIf{   y(_) }

    val phi = 1.0 / m * y_ones
    val mu0 = mu0_num.map{ _ / y_zeros }
    val mu1 = mu1_num.map{ _ / y_ones }

    val sigma = sum(0, m)(n,n){i => 
      //if (y(i)) (x.sliceRow(i) - mu1) ** (x.sliceRow(i) - mu1)
      //else      (x.sliceRow(i) - mu0) ** (x.sliceRow(i) - mu0)
      val mu = if (y(i)) mu1 else mu0
      (x.sliceRow(i) - mu) ** (x.sliceRow(i) - mu)
    }

    toc(sigma)
    println("  phi = " + phi)
    println("  mu0 = "); mu0.slice(0,10).pprint
    println("  mu1 = "); mu1.slice(0,10).pprint
    sigma.slice(0,4,0,4).pprint
  }
}

object GDALiteCompiler extends PPLCompiler with GDALite
trait GDALite extends PPLApp {
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

    val sigma = reduce(m, collect(n,n){(i,j) => 0.0}){r => 
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
    val zeroMat = collect(n,n){(i,j) => 0.0}
    
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
