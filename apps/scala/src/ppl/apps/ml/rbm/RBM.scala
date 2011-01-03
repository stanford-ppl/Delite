package ppl.apps.ml.rbm

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object RBM extends DeliteApplication with OptiMLExp {

  def print_usage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
    exit(-1)
  }

  def main() = {

    if (args.length < 3) print_usage

    val maxEpoch = 10 // maximum number of epochs
    val numHiddenUnits = Integer.parseInt(args(1))

    val epsilonw = 0.1f // Learning rate for weights
    val epsilonvb = 0.1f // Learning rate for biases of visible units
    val epsilonhb = 0.1f //Learning rate for biases of hidden units
    val weightcost = 0.0002f
    val initialmomentum = 0.5f
    val finalmomentum = 0.9f

    println("Using " + numHiddenUnits + " hidden units.")
    
    println("Reading MNIST dataset")
    val numcases = Integer.parseInt(args(2)) // batchsize
    //val numcases = 100 // batchsize
    val trainingdata = MLInputReader.read(args(0)).toFloat(e => e.floatValue())
    val numdims = trainingdata.numCols
    val numbatches = trainingdata.numRows / numcases

    // Initialize symmetric weights and biases
    var vishid = Matrix.randnf(numdims, numHiddenUnits) * 0.1f
    var hidbiases = Vector.zerosf(numHiddenUnits)
    var visbiases = Vector.zerosf(numdims)

    var vishidinc = Matrix.zerosf(numdims, numHiddenUnits)
    var hidbiasinc = Vector.zerosf(numHiddenUnits)
    var visbiasinc = Vector.zerosf(numdims)

    for (epoch <- 0 until maxEpoch) {
      //var errsum = DeliteFloat(0)
      for (batch <- 0 until numbatches) {
        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        //PerformanceTimer.start("RBM-posphase", false)
        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
        val poshidprobs = (data * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
        val posprods = data.t * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol
        //val poshidstates = (poshidprobs :< Matrix.randf(numcases, numHiddenUnits))
        val poshidstates = (poshidprobs zip Matrix.randf(numcases, numHiddenUnits)){ (a,b) => if (a < b) 0f else 1f }
        //PerformanceTimer.stop("RBM-posphase", false)

        // Negative phase
        //PerformanceTimer.start("RBM-negphase", false)
        val negdata = (poshidstates * vishid.t + visbiases.replicate(numcases, 1)).sigmoidf
        val neghidprobs = (negdata * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
        val negprods = negdata.t * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol
        val diff = data - negdata
        //errsum += (diff dot diff).sum[DeliteFloat]
        //PerformanceTimer.stop("RBM-negphase", false)

        // Update weights and biases
        //PerformanceTimer.start("RBM-biasupdates", false)
        val momentum = if (epoch > 5) finalmomentum else initialmomentum
        vishidinc = vishidinc * momentum + ((posprods - negprods) / numcases  - (vishid * weightcost))*epsilonw
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
        //PerformanceTimer.stop("RBM-biasupdates", false)
      }
      //println("--> Epoch " + epoch + " error = " + errsum.value)
    }

    //PerformanceTimer.print("RBM-posphase")
    //PerformanceTimer.save("RBM-posphase")
    //PerformanceTimer.print("RBM-negphase")
    //PerformanceTimer.save("RBM-negphase")
    //PerformanceTimer.print("RBM-biasupdates")
    //PerformanceTimer.save("RBM-biasupdates")
  }
}
