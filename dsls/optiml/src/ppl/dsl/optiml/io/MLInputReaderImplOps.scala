package ppl.dsl.optiml.io

import java.io._
import scala.virtualization.lms.common.Base
import ppl.dsl.optiml._
import ppl.dsl.optiml.application.BinarizedGradientTemplate

trait MLInputReaderImplOps { this: Base =>
  def mlinput_read_grayscale_image_impl(filename: Rep[String]): Rep[GrayscaleImage]
  def mlinput_read_arff_impl[Row:Manifest](filename: Rep[String], schemaBldr: Rep[DenseVector[String]] => Rep[Row]): Rep[DenseVector[Row]]
  //def mlinput_read_tokenmatrix_impl(filename: Rep[String]): Rep[TrainingSet[Double,Double]]
  def mlinput_read_tokenmatrix_impl(filename: Rep[String]): (Rep[DenseMatrix[Double]],Rep[DenseVector[Double]])
  def mlinput_read_template_models_impl(directory: Rep[String]): Rep[DenseVector[(String, DenseVector[BinarizedGradientTemplate])]]
}

trait MLInputReaderImplOpsStandard extends MLInputReaderImplOps {
  this: OptiMLCompiler with OptiMLLift =>
  
  ///////////////
  // kernels

  def mlinput_read_grayscale_image_impl(filename: Rep[String]): Rep[GrayscaleImage] = {
    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    var ints = line.split("\\\\s+")
    val x = DenseMatrix[Int](0, ints.length)

    while (line != null) {
      val v = (0::ints.length) { i =>
        Integer.parseInt(ints(i))
      }
      x += v.unsafeImmutable

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        ints = line.split("\\\\s+")
      }
    }
    xfs.close()

    GrayscaleImage(x.unsafeImmutable)
  }


  def mlinput_read_arff_impl[Row:Manifest](filename: Rep[String], schemaBldr: Rep[DenseVector[String]] => Rep[Row]): Rep[DenseVector[Row]] = {
    val xfs = BufferedReader(FileReader(filename))    
    
    // skip past the header to the data section
    // since we are using schemaBldr, we don't care about the attribute types
    var line = xfs.readLine()
    while (!line.startsWith("@DATA") && line != null) {
      line = xfs.readLine()
    }
    
    val out = DenseVector[Row](0, false)
    
    if (line != null) {
      line = xfs.readLine()
      while (line != null) {
        line = line.trim()
        if (!line.startsWith("%")) {
           val row = line.split(",")
           val schemaData = DenseVector[String](0, true)         
           for (e <- row) {
             schemaData += e
           }
           out += schemaBldr(schemaData)
        }
        line = xfs.readLine()
      }
    }
    
    out
  }
        

 /* the input file is expected to follow the format:
  *  <header>
  *  <num documents> <num tokens>
  *  <tokenlist>
  *  <document word matrix, where each row repesents a document and each column a distinct token>
  *    each line of the doc word matrix begins with class (0 or 1) and ends with -1
  *    the matrix is sparse, so each row has a tuple of (tokenIndex, number of appearances)
  */
  //def mlinput_read_tokenmatrix_impl(filename: Rep[String]): Rep[TrainingSet[Double,Double]] = {
  def mlinput_read_tokenmatrix_impl(filename: Rep[String]): (Rep[DenseMatrix[Double]], Rep[DenseVector[Double]]) = {

    val xs = BufferedReader(FileReader(filename))

    // header and metadata
    var header = xs.readLine()

    var line = xs.readLine()
    val counts = line.trim().split("\\\\s+")
    val numDocs = Integer.parseInt(counts(0))
    val numTokens = Integer.parseInt(counts(1))
    if ((numDocs < 0) || (numTokens < 0)) {
      error("Illegal input to readTokenMatrix")
    }

    // tokens
    val tokenlist = xs.readLine()

    val trainCatSeq = DenseVector[Double](0,true)
    val trainMatSeq = DenseVector[DenseVector[Double]](0, true)

    for (m <- 0 until numDocs) {
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\\\s+")

      val row = DenseVector[Double](numTokens,true)
      var cumsum = unit(0); var j = unit(1)
      // this could be vectorized
      while (j < repArithToArithOps(nums.length) - 1){
        cumsum += Integer.parseInt(nums(j))
        row(cumsum) = Double.parseDouble(nums(j+1))
        j += 2
      }
      trainCatSeq += Double.parseDouble(nums(0))
      trainMatSeq += row.unsafeImmutable
    }
    val trainCategory = trainCatSeq.t
    val trainMatrix = DenseMatrix(trainMatSeq)

    xs.close()

    //return (trainMatrix,tokenlist,trainCategory)
    (trainMatrix.unsafeImmutable, trainCategory.unsafeImmutable)
    //TrainingSet[Double,Double](trainMatrix.unsafeImmutable, Labels(trainCategory.unsafeImmutable))
  }

  def mlinput_read_template_models_impl(directory: Rep[String]): Rep[DenseVector[(String, DenseVector[BinarizedGradientTemplate])]] = {
    val templateFiles = DenseVector[String](0, true)
    for (f <- File(directory).getCanonicalFile.listFiles) {
      templateFiles += f.getPath()
    }

    templateFiles.map { filename =>
      println("Loading model: " + filename)
      val templates = DenseVector[BinarizedGradientTemplate](0, true)

      val file = BufferedReader(FileReader(filename))

      if (file.readLine() != "bigg_object:") error("Illegal data format")
      file.readLine() //"============"
      val params = file.readLine().trim.split(" ")
      if (params(0) != "obj_name/obj_num/num_objs:") error("Illegal data format")
      val objName = params(1)
      val objId = params(2)
      val numObjs = Integer.parseInt(params(3))
      var i = unit(0)
      while (i < numObjs) {
        templates += loadModel(file)
        i += 1
      }
      (objName, templates.unsafeImmutable)
    }
  }

  private def loadModel(file: Rep[BufferedReader]): Rep[BinarizedGradientTemplate] = {
    if (file.readLine().trim != "====OneBiGG====:") error("Illegal data format")
    var temp = file.readLine().trim.split(" ")
    if (temp(0) != "view/radius/reduction:") error("Illegal data format")
    val view = Integer.parseInt(temp(1))
    val radius = Integer.parseInt(temp(2))
    val reductionFactor = Integer.parseInt(temp(3))

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Gradients:") error("Illegal data format")
    val gradientsSize = Integer.parseInt(temp(1))
    val gradients = DenseVector[Int](gradientsSize,true)
    val gradientsString = file.readLine().trim.split(" ")
    var i = unit(0)
    while (i < gradientsSize) {
      gradients(i) = Integer.parseInt(gradientsString(i))
      i += 1
    }

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Match_list:") error("Illegal data format")
    val matchListSize = Integer.parseInt(temp(1))
    val matchList = IndexVector(0)
    val matchListString = file.readLine().trim.split(" ")
    i = 0
    while (i < matchListSize) {
      matchList += Integer.parseInt(matchListString(i)) //TODO TR matchList not mutable?
      i += 1
    }

    temp = file.readLine().trim.split(" ")
    if (temp(0) != "Occlusions:") error("Illegal data format")
    val occlusionsSize = Integer.parseInt(temp(1))
    val occlusions = DenseVector[DenseVector[Int]](0,true)
    val occlusionsString = file.readLine().trim.split(" ")
    if (occlusionsSize != 0) error("Occlusions not supported.")

    if (file.readLine().trim != "BoundingBox:") error("Illegal data format")
    val bbString = file.readLine().trim.split(" ")
    val x = Integer.parseInt(bbString(0))
    val y = Integer.parseInt(bbString(1))
    val width = Integer.parseInt(bbString(2))
    val height = Integer.parseInt(bbString(3))
    val bb = Rect(x, y, width, height)

    // TODO: Anand, should not be initializing these null unless we add setters to BinarizedGradientTemplate
    BinarizedGradientTemplate(radius, bb, null, 0, gradients, matchList, occlusions, null, null)
  }

}
