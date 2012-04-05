package ppl.apps.ml.nb

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

/*
object NaiveBayesVectorizedRunner extends OptiMLApplicationRunner with NaiveBayesVectorized
trait NaiveBayesVectorized extends OptiMLApplication {
	
  def print_usage = {
    println("NaiveBayes <training file> <test file>")
    exit(-1)
  }

  def main() = {
    println("Naive Bayes Example Application")

    if (args.length < 2) print_usage

    val trainingFile = args(0)
    val testFile = args(1)

    // Train Model
    val trainingSet = MLInputReader.readTokenMatrix(trainingFile)
    //val start_train = System.currentTimeMillis()
    tic(trainingSet)
    val (phi_y1, phi_y0, phi_y) = train(trainingSet)
    toc(phi_y1)

    // test
    val testSet = MLInputReader.readTokenMatrix(testFile)
    println("phi_y1: "); phi_y1.pprint; println("phi_y0: "); phi_y0.pprint; println("phi_y: "+ phi_y)
    val incorrect_classifications = test(testSet, phi_y1, phi_y0, phi_y)
    println("Test error: " + incorrect_classifications.doubleValue() / testSet.numSamples.doubleValue())

    //PerformanceTimer.save("NaiveBayes")
  }

  def train(ts: Rep[TrainingSet[Double,Double]]) : (Rep[DenseVector[Double]], Rep[DenseVector[Double]], Rep[Double]) = {
    val numTrainDocs = ts.numSamples
    val numTokens = ts.numFeatures

    val words_per_email = (0::ts.numSamples){ i => ts(i).sum }

    println("Training model on " + numTrainDocs + " documents.")

    val spamcount = ts.labels.sum

    val spamwordcount = Vector.zeros(numTokens)
    val spam_totalwords = Vector.zeros(numTokens)
    val nonspamwordcount = Vector.zeros(numTokens)
    val nonspam_totalwords = Vector.zeros(numTokens)

    var i = unit(0)

    while (i < numTrainDocs) {
      //val words = words_per_email(i)
      if (ts.labels(i) == 1){
        spamwordcount += ts(i)
                  spam_totalwords.mmap(ele => ele + words_per_email(i))
      }
      else {
        nonspamwordcount += ts(i)
        nonspam_totalwords.mmap(ele => ele + words_per_email(i))
      }
      i += 1
    }
    val phi_y1 = (spamwordcount + 1) / (spam_totalwords + numTokens)
    val phi_y0 = (nonspamwordcount + 1) / (nonspam_totalwords + numTokens)

    val phi_y = spamcount / numTrainDocs

    (phi_y1, phi_y0, phi_y)
  }

  def test(ts: Rep[TrainingSet[Double,Double]], phi_y1: Rep[DenseVector[Double]], phi_y0: Rep[DenseVector[Double]], phi_y: Rep[Double]): Rep[Int] = {
    val numTestDocs = ts.numSamples
    val numTokens = ts.numFeatures

    println("Testing model on " + numTestDocs + " documents.")

    val output = (0::numTestDocs){j => {
      // compute log(p(x|y=1)p(y=1)) and log(p(x|y=0)p(y=0))
      val (p_norm, p_spam) = t2( sum(0,numTokens) { i =>
        if (ts(j,i) > 0){
          ((log(phi_y0(i)) + log(1.-phi_y)) * ts(j,i),
           (log(phi_y1(i)) + log(phi_y)) * ts(j,i))
        }
        else{
          (unit(0.0), unit(0.0))
        }
      })

      if (p_spam > p_norm){
        1.
      }
      else{
        0.
      }
    }}

    // Compute error on test set
    var incorrect_classifications = unit(0)
		var i = 0
		while (i < numTestDocs){
      if (ts.labels(i) != output(i))
        incorrect_classifications += 1
			i += 1
    }
    incorrect_classifications
  }
}
*/