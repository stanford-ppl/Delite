package asplos

import scala.reflect.SourceContext
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

trait MultiArrayOps extends DeliteMultiArrayOps with DeliteSimpleOps { this: SMALApp => 
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
    def count(cond: Rep[T] => Rep[Boolean]): Rep[Int] = filterReduce(x.length, 0){i => !cond(x(i)) }{i => 1}{_+_} //x.map{e => if (f(e)) 1 else 0}.reduce(0){_+_}
    def pprint: Rep[Unit] = println(x.mkString(" ")) 
  }

  implicit def array1d_double_ops(ma: Rep[DeliteArray1D[Double]])(implicit ctx: SourceContext) = new Array1DDoubleOpsCls(ma)
  class Array1DDoubleOpsCls(x: Rep[Array1D[Double]])(implicit ctx: SourceContext) {
    def ** (y: Rep[Array1D[Double]]): Rep[Array2D[Double]]
      = Array2D.fromFunction(x.length, y.length){(i,j) => x(i) * y(j) }
     
    def - (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_-_}
    def + (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_+_}
    def * (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_*_}
    def / (y: Rep[Array1D[Double]]): Rep[Array1D[Double]] = x.zip(y){_/_}
    def += (y: Rep[Array1D[Double]]): Rep[Unit] = x.mzip(y){_+_}

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
    def += (y: Rep[Array2D[Double]]): Rep[Unit] = x.mzip(y){_+_}
    def sum: Rep[Double] = x.reduce(0.0){_+_}

    // TODO: This is not safe as a parallel mutable reduce
    def sumRowsIf(cond: Rep[Int] => Rep[Boolean]): Rep[Array1D[Double]] 
      = filterReduce[Array1D[Double]](x.nRows, zeros(x.nCols)){i => !cond(i)}{i => x.sliceRow(i)}{_+_}
  }

  def read1D(path: Rep[String])(implicit ctx: SourceContext): Rep[Array1D[Double]] = { Array1D.fromFile(path){s => parse_double(s) } }
  def read2D(path: Rep[String])(implicit ctx: SourceContext): Rep[Array2D[Double]] = { Array2D.fromFile(path){s => parse_double(s) } }

  def sum(start: Rep[Int], end: Rep[Int])(nRows: Rep[Int], nCols: Rep[Int])(f: Rep[Int] => Rep[Array2D[Double]])(implicit ctx: SourceContext)
    = mreduce[Array2D[Double]](end - start, () => zeros(nRows,nCols).unsafeMutable ){i => f(i)}{(a,b) => a += b; a}
}
