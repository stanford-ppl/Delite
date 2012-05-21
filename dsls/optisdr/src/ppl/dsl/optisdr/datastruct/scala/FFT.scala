package ppl.dsl.optisdr.datastruct.scala

import edu.emory.mathcs.jtransforms.fft._
import ppl.dsl.optila.datastruct.scala.DenseVector

object FFT {
  def complexToDoubles(cpxArray: Array[Complex]) = {
    val flatArray = new Array[Double](cpxArray.length*2)
    
    for(i <- 0 until cpxArray.length) {
      flatArray(2*i) = cpxArray(i).real
      flatArray(2*i+1) = cpxArray(i).imag
    }
    
    flatArray
  }
  
  def doublesToComplexVector(dblArray: Array[Double]) = {
    val result = new DenseVector[Complex](dblArray.length / 2, false)
    
    for(i <- 0 until dblArray.length / 2) {
      result._data(i) = Complex(dblArray(2*i), dblArray(2*i+1))
    }
    
    result
  }

  def complexForward(cpxVector: DenseVector[Complex]) = {
    val doubleArray = complexToDoubles(cpxVector._data)
  
    val doubleFFT = new DoubleFFT_1D(cpxVector._length)
    doubleFFT.complexForward(doubleArray)
    
    doublesToComplexVector(doubleArray)
  }
  
  def complexInverse(cpxVector: DenseVector[Complex], scale: Boolean = false) = {
    val doubleArray = complexToDoubles(cpxVector._data)
  
    val doubleFFT = new DoubleFFT_1D(cpxVector._length)
    doubleFFT.complexInverse(doubleArray, scale)
    
    doublesToComplexVector(doubleArray)
  }
  
  def padRealArray(realArray: Array[Double]) = {
    val paddedArray = new Array[Double](realArray.length*2)
    
    Array.copy(realArray, 0, paddedArray, 0, realArray.length)
    
    paddedArray
  }
  
  def realForward(realVector: DenseVector[Double]) = {
    val paddedArray = padRealArray(realVector._data)
  
    val doubleFFT = new DoubleFFT_1D(realVector._length)
    doubleFFT.realForwardFull(paddedArray)
    
    doublesToComplexVector(paddedArray)
  }
  
  def realInverse(realVector: DenseVector[Double], scale: Boolean = false) = {
    val paddedArray = padRealArray(realVector._data)
  
    val doubleFFT = new DoubleFFT_1D(realVector._length)
    doubleFFT.realInverseFull(paddedArray, scale)
    
    doublesToComplexVector(paddedArray)
  }
}
