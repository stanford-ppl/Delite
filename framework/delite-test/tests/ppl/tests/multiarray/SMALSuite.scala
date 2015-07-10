package ppl.tests.multiarray

import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.hw._
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.Config

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

// Code generators
trait SmalScalaCodegen extends DeliteTestDSLCodeGenScala with ScalaGenMultiArray with ScalaGenSimpleProfileOps {val IR: SmalRunner}
trait SmalCudaCodegen extends DeliteTestDSLCodeGenCuda with CudaGenMultiArray with CudaGenSimpleProfileOps {val IR: SmalRunner}
trait SmalCCodegen extends DeliteTestDSLCodeGenC with CGenMultiArray with CGenSimpleProfileOps {val IR: SmalRunner}
trait SmalHWCodegen extends DeliteTestDSLCodeGenHw {val IR: SmalRunner}

trait SmalRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with MultiArrayTransform with SimpleProfileOpsExp {
  override def getCodeGenPkg(t: Target{val IR: SmalRunner.this.type}) : GenericFatCodegen{val IR: SmalRunner.this.type} = {
    t match {
      case _:TargetScala => new SmalScalaCodegen{val IR: SmalRunner.this.type = SmalRunner.this}
      case _:TargetCuda => new SmalCudaCodegen{val IR: SmalRunner.this.type = SmalRunner.this}
      case _:TargetCpp => new SmalCCodegen{val IR: SmalRunner.this.type = SmalRunner.this}
      case _:TargetHw => new SmalHWCodegen{val IR: SmalRunner.this.type = SmalRunner.this}
      case _ => throw new Exception("SMALRunner does not support this target")
    }
  }  
}

trait SmalApp extends DeliteTestModule with DeliteTestDSLApplication 
  with DeliteMultiArrayOps with DeliteSimpleOps with SimpleProfileOps {
  
  type MultiArray[T] = DeliteMultiArray[T]
  type Array1D[T] = DeliteArray1D[T]
  type Array2D[T] = DeliteArray2D[T]
  type Array3D[T] = DeliteArray3D[T]
  type Array4D[T] = DeliteArray4D[T]
  type Array5D[T] = DeliteArray5D[T]

  implicit def multiArrayManifest[T:Manifest] = manifest[DeliteMultiArray[T]]
  implicit def array1DManifest[T:Manifest] = manifest[DeliteArray1D[T]]
  implicit def array2DManifest[T:Manifest] = manifest[DeliteArray2D[T]]
  implicit def array3DManifest[T:Manifest] = manifest[DeliteArray3D[T]]
  implicit def array4DManifest[T:Manifest] = manifest[DeliteArray4D[T]]
  implicit def array5DManifest[T:Manifest] = manifest[DeliteArray5D[T]]
  
  implicit def array1DtoMultiArray[T:Manifest](x: Rep[DeliteArray1D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array2DtoMultiArray[T:Manifest](x: Rep[DeliteArray2D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array3DtoMultiArray[T:Manifest](x: Rep[DeliteArray3D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array4DtoMultiArray[T:Manifest](x: Rep[DeliteArray4D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]
  implicit def array5DtoMultiArray[T:Manifest](x: Rep[DeliteArray5D[T]]) = x.asInstanceOf[Rep[DeliteMultiArray[T]]]

  def zeros(len: Rep[Int])(implicit ctx: SourceContext): Rep[Array1D[Double]] = Array1D.fromFunction(len){i => unit(0.0)}
  def zeros(nRows: Rep[Int], nCols: Rep[Int])(implicit ctx: SourceContext): Rep[Array2D[Double]] = Array2D.fromFunction(nRows,nCols){(i,j) => unit(0.0)}

  def parse_double(s: Rep[String])(implicit ctx: SourceContext): Rep[Double] = { s.toDouble }

  implicit def array1d_extras[T:Manifest](ma: Rep[Array1D[T]])(implicit ctx: SourceContext) = new Array1DExtraOpsCls(ma)
  class Array1DExtraOpsCls[T:Manifest](x: Rep[Array1D[T]])(implicit ctx: SourceContext) {
    def count(f: Rep[T] => Rep[Boolean]): Rep[Int] = x.map{e => if (f(e)) 1 else 0}.reduce(0){_+_}
    def pprint: Rep[Unit] = println(x.mkString(" ")) 
  }

  implicit def array1d_double_ops(ma: Rep[DeliteArray1D[Double]])(implicit ctx: SourceContext) = new Array1DDoubleOpsCls(ma)
  class Array1DDoubleOpsCls(x: Rep[Array1D[Double]])(implicit ctx: SourceContext) {
    def ** (y: Rep[Array1D[Double]]): Rep[Array2D[Double]]
      = Array2D.fromFunction(x.length, x.length){(i,j) => x(i) * y(j) }
     
    def - (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_-_}
    def + (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_+_}
    def * (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_*_}
    def / (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_/_}

    // TODO: Need overload hacks for these to work
    //def - (y: Rep[Double]): Rep[DeliteArray1D[Double]] = x.map{_ - y}
    //def + (y: Rep[Double]): Rep[DeliteArray1D[Double]] = x.map{_ + y}
    //def * (y: Rep[Double]): Rep[DeliteArray1D[Double]] = x.map{_ * y}
    //def / (y: Rep[Double]): Rep[DeliteArray1D[Double]] = x.map{_ / y}
    def sum: Rep[Double] = x.reduce(0.0){_+_}
  }

  implicit def array2d_extras[T:Manifest](ma: Rep[Array2D[T]]) = new Array2DExtraOpsCls(ma)
  class Array2DExtraOpsCls[T:Manifest](x: Rep[Array2D[T]]) {
    def pprint: Rep[Unit] = println(x.mkString("\n", " "))
  }

  implicit def array2d_double_ops(ma: Rep[Array2D[Double]])(implicit ctx: SourceContext) = new Array2DDoubleOpsCls(ma)
  class Array2DDoubleOpsCls(x: Rep[Array2D[Double]])(implicit ctx: SourceContext) {
    def - (y: Rep[Array2D[Double]]): Rep[Array2D[Double]] = x.zip(y){_-_}
    def + (y: Rep[Array2D[Double]]): Rep[Array2D[Double]] = x.zip(y){_+_}
    def * (y: Rep[Array2D[Double]]): Rep[Array2D[Double]] = x.zip(y){_*_}
    def / (y: Rep[Array2D[Double]]): Rep[Array2D[Double]] = x.zip(y){_/_}
    def sum: Rep[Double] = x.reduce(0.0){_+_}

    def sumRowsIf(filter: Rep[Int] => Rep[Boolean]): Rep[Array1D[Double]] 
      = filterReduce[Array1D[Double]](x.nRows, zeros(x.nCols))(filter){x.sliceRow(_)}{_+_}
  }

  def read1D(path: Rep[String])(implicit ctx: SourceContext): Rep[Array1D[Double]] = { Array1D.fromFile(path){s => parse_double(s) } }
  def read2D(path: Rep[String])(implicit ctx: SourceContext): Rep[Array2D[Double]] = { Array2D.fromFile(path){s => parse_double(s) } }

  def sum(start: Rep[Int], end: Rep[Int])(nRows: Rep[Int], nCols: Rep[Int])(f: Rep[Int] => Rep[Array2D[Double]])(implicit ctx: SourceContext)
    = reduce[Array2D[Double]](end - start, zeros(nRows,nCols)){i => f(i)}{_+_}
}

class SmalSuite extends DeliteSuite {

  def runBench(app: DeliteTestRunner, args: Array[String], target: String, threads: Int) {
    println("RUNNING")
    ppl.delite.runtime.Delite.embeddedMain(args, app.staticDataMap)
  }

  def stageBench(app: DeliteTestRunner) = {
    println("STAGING")
    val saveDeg = Config.degFilename
    val saveBuildDir = Config.buildDir
    val saveCacheSyms = Config.cacheSyms
    val generatedDir = Config.buildDir + java.io.File.separator + uniqueTestName(app)
    try {
      Config.degFilename = degName(app)
      Config.buildDir = generatedDir
      Config.cacheSyms = cacheSyms
      app.main(Array())
    } finally {
      // concurrent access check
      assert(Config.buildDir == generatedDir)
      Config.degFilename = saveDeg
      Config.buildDir = saveBuildDir
      Config.cacheSyms = saveCacheSyms
    }
  }

  def compileAndRun(app: DeliteTestRunner) {
    validateParameters()
    val args = Array(degName(app))

    for(t <- deliteTestTargets) {
      t match {
        case "scala" =>
        case "cuda" => Config.generateCUDA = true; Config.generateCpp = true
        case "cpp" => Config.generateCpp = true
        case _ => println("Unknown test target: " + t)
      }
    }
    if (useBlas) Config.useBlas = true

    stageBench(app)

    // Set runtime parameters for targets and execute runtime
    for(target <- deliteTestTargets) {
      for (num <- threads) {
        def runtimeConfig(numScala: Int = 1, numCpp: Int = 0, numCuda: Int = 0, numOpenCL: Int = 0) {
          ppl.delite.runtime.Config.numThreads = numScala
          ppl.delite.runtime.Config.numCpp = numCpp
          ppl.delite.runtime.Config.numCuda = numCuda
        }

        target match {
          case "scala" => runtimeConfig(numScala = num)
          case "cpp" => runtimeConfig(numCpp = num)
          case "cuda" => runtimeConfig(numScala = num, numCuda = 1) //scala or cpp (or both) for host?
          case _ => assert(false)
        }
        runBench(app, args, target, num)
      }
    }

  }

  // Printing the result of a map-reduce doesn't work right now - gets a missing op error at runtime
  /*def testDeliteReduce = {
    trait DeliteReduceTest extends SmalApp {
      def test() = {
        val zero = DeliteArray.imm[Double](5)
        val vecs = DeliteArray.fromFunction(5){i => DeliteArray.fromFunction(5){i => i.toDouble} }
        val reduc = vecs.reduce({(a,b) => a.zip(b){_+_}}, zero)
        
        val vec = DeliteArray.fromFunction(5){i => 0.0}
        val full = vec.zip(reduc){(a,b) => a + b}
        println(full(0))
      }
    }
    compileAndTest( new SmalRunner with DeliteReduceTest )
  }*/

  /*def testBranch = {
    trait BranchTest extends SmalApp {
      def test() = {
        val r = Array1D.fromFunction(10){i => i}.reduce(0){_+_}
        val mu = if (r > 3) Array1D.fromFunction(10){i => 10 - i} 
                 else       Array1D.fromFunction(10){i => 10 + i}

        mu.pprint
      }
    }
    compileAndTest( new SmalRunner with BranchTest )
  }*/

  /*def testSum = {
    trait SumTest extends SmalApp {
      def test() = {
        val x = Array2D.fromFunction(256,16){(i,j) => (i + j).toDouble }
        val r = sum(0,x.nRows/16)(16,16){i => x.slice(i*16,16,0,16)}
        println(r.nRows)
        println(r.nCols)
        r.pprint
      }
    }
    compileAndTest( new SmalRunner with SumTest )
  }*/

  /*def testSumRowsIf = {
    trait SumRowsIfTest extends SmalApp {
      def test() = {
        val x = read2D("/home/david/PPL/data/1024-1200x.dat")
        val y = read1D("/home/david/PPL/data/q1y.dat").map{d => d > 0.0}
        val mu0 = x.sumRowsIf{ !y(_) }

        mu0.slice(0,5).pprint
      }
    }
    compileAndTest( new SmalRunner with SumRowsIfTest )
  }*/

  def testGDA = {
    trait GDA extends SmalApp {
      def main() {
        val x = read2D("/home/david/PPL/data/1024-1200x.dat")
        val y = read1D("/home/david/PPL/data/q1y.dat").map{d => d <= 0.0}

        tic()

        val m = y.length // Number of training samples
        val n = x.nCols  // Dimesionality of training data

        println("n = " + n)
        println("m = " + m)
        val y_zeros = y count {_ == false}
        val y_ones = y count {_ == true}
        val mu0_num = x.sumRowsIf{ !y(_) }
        val mu1_num = x.sumRowsIf{  y(_) }

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
        println(sigma(0,0))
        /*
        println("  mu0 = " ); mu0.pprint
        println("  mu1 = " ); mu1.pprint
        println("  sigma = "); sigma.sliceRows(0,10).pprint*/
      }
    }
    compileAndRun(new SmalRunner with GDA )
  }

  /*def testGDABlocked = {
    trait GDABlocked extends SmalApp {
      def test() = {

      }
    }
    compileAndTest(new SmalRunner with GDABlocked)
  }*/

} /* End of SmalSuite */
