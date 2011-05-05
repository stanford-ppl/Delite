package ppl.apps.ml.svm

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object SVM extends DeliteApplication with OptiMLExp {

  def print_usage = {
    println("Usage: SVM <train data file> <test data file> <model filename> <num tests>")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) print_usage

    val trainfile = args(0)
    val testfile = args(1)
    val modelFile = args(2)
    val numTests = Integer.parseInt(args(3))

    //val f1 = File(trainfile)
    //val f2 = File(testfile)
    //if (!f1.exists || !f2.exists) print_usage

    reseed

    // parse the input matrix into the following elements:
    //  inMatrix:   a (numDocs x numTokens) matrix, where each row represents a unique document
    //                 the jth column of row i represents the number of times the jth token appeared in doc i
    //  tokenlist:  a long string containing the list of all tokens (words)
    //  inCategory: a (numDocs x 1) vector containing the true classifications for the documents just read
    //                 the ith entry gives the correct class for the ith email, where spam is 1 and non-spam is 0.
    val inMatrixTrain = MLInputReader.readTokenMatrix(trainfile)
    val inMatrixTest = MLInputReader.readTokenMatrix(testfile)

    // adjust the classification labels to -1 and +1 for SMO
    inMatrixTrain.labels mmap { e => if (e == 0) -1; else 1 }
    inMatrixTest.labels mmap { e => if (e == 0) -1; else 1 }

    // run the SMO training algorithm
    val svm = new SVMModel { val IR = SVM.this }
    //val svm = new SVMModel(SVM.this)
    tic
    val (weights, b) = svm.train(inMatrixTrain, 1, .001, 10)
    toc
    //svm.computeWeights(inMatrixTrain, YTrain)
    //svm.saveModel(weights, b, modelFile)

    //println("SVM training finished. Model saved to " + modelFile)

    // TEST RESULTS
    val YTest = inMatrixTest.labels
    val numTestDocs = inMatrixTest.numRows
    //svm.load(modelFile)
    val outputLabels = (0::numTestDocs){ i => svm.classify(weights, b, inMatrixTest(i)) }
    println("SVM testing finished. Calculating error..")
    var errors = unit(0)
    for (i <- 0 until numTestDocs){
      if (YTest(i) != outputLabels(i)) errors +=1
      //println("predicted class: " + outputLabels(i) + ", actual: " + Y(i))
    }
    println("Classification error: " + (errors.doubleValue()/numTestDocs.doubleValue()))
  }
}
