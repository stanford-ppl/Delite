package ppl.dsl.optiml

import datastruct.scala.{Vector,Matrix,TrainingSet}
import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.ScalaOpsPkg

trait MLInputReaderImplOps { this: Base =>
  def mlinput_read_impl(filename: Rep[String]) : Rep[Matrix[Double]]
  def mlinput_read_vector_impl(filename : Rep[String]) : Rep[Vector[Double]]
  def mlinput_read_tokenmatrix_impl(filename: Rep[String]): Rep[TrainingSet[Double,Double]]
}

trait MLInputReaderImplOpsStandard extends MLInputReaderImplOps {
  this: OptiML =>
  
  ///////////////
  // kernels

  def mlinput_read_impl(filename: Rep[String]) = {
    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    // TODO: weirdness with StringOps, losing a \        
    var dbls = line.split("\\\\s+")
    val x = Matrix[Double](0, dbls.length)

    while (line != null){
      val v = Vector[Double](dbls.length, true)
      for (i <- 0 until dbls.length){
        v(i) = Double.parseDouble(dbls(i))
      }
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        dbls = line.split("\\\\s+")
      }
    }
    xfs.close()

    x
  }

  def mlinput_read_vector_impl(filename: Rep[String]) = {
    val x = Vector[Double](0, true)

    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    while (line != null){
      line = line.trim()
      val dbl = Double.parseDouble(line)
      x += dbl

      line = xfs.readLine()
    }
    xfs.close()

    x
  }

 /* the input file is expected to follow the format:
  *  <header>
  *  <num documents> <num tokens>
  *  <tokenlist>
  *  <document word matrix, where each row repesents a document and each column a distinct token>
  *    each line of the doc word matrix begins with class (0 or 1) and ends with -1
  *    the matrix is sparse, so each row has a tuple of (tokenIndex, number of appearances)
  */
  def mlinput_read_tokenmatrix_impl(filename: Rep[String]): Rep[TrainingSet[Double,Double]] = {

    var xs = BufferedReader(FileReader(filename))

    // header and metadata
    var header = xs.readLine()

    var line = xs.readLine()
    val counts = line.trim().split("\\s+")
    val numDocs = Integer.parseInt(counts(0))
    val numTokens = Integer.parseInt(counts(1))
    if ((numDocs < 0) || (numTokens < 0)) {
      //throw new RuntimeException("Illegal input to readTokenMatrix")
      println("Illegal input to readTokenMatrix")
      exit(0)
    }

    // tokens
    val tokenlist = xs.readLine()

    val trainCatSeq = Vector[Double]()
    for (m <- 0 until numDocs){
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\s+")

      trainCatSeq += Double.parseDouble(nums(0))
    }
    val trainCategory = trainCatSeq.t

    xs.close()
    xs = BufferedReader(FileReader(filename))
    xs.readLine(); xs.readLine(); xs.readLine()

    val trainMatSeq = Vector[Vector[Double]](0, true)
    for (m <- 0 until numDocs) {
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\s+")

      var row = Vector[Double](numTokens,true)
      var cumsum = unit(0); var j = unit(1)
      // this could be vectorized
      while (j < nums.length - 1){
        cumsum += Integer.parseInt(nums(j))
        row(cumsum) = Double.parseDouble(nums(j+1))
        j += 2
      }
      trainMatSeq += row
    }
    val trainMatrix = Matrix(trainMatSeq)

    xs.close()

    //return (trainMatrix,tokenlist,trainCategory)
    return TrainingSet[Double,Double](trainMatrix, Labels(trainCategory))
  }

}