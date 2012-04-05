package ppl.apps.ml.rbm

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object RBMRunnerNCNR extends OptiMLApplicationRunnerBase with OptiMLNoCSE with OptiMLExp with RBM
object RBMRunnerNC extends OptiMLApplicationRunner with OptiMLNoCSE with RBM
object RBMRunnerNR extends OptiMLApplicationRunnerBase with OptiMLExp with RBM
object RBMRunner extends OptiMLApplicationRunner with RBM

trait RBM extends OptiMLApplication {

  def print_usage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
    exit(-1)
  }

  def main() = {
    reseed

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
    val trainingdata = readMatrix(args(0)).toFloat(e => e.floatValue())
    val numdims = trainingdata.numCols
    val numbatches = trainingdata.numRows / numcases

    // Initialize symmetric weights and biases
    val vishid = (Matrix.randnf(numdims, numHiddenUnits) * 0.1f).mutable
    val hidbiases = Vector.zerosf(numHiddenUnits).mutable
    val visbiases = Vector.zerosf(numdims).mutable

    var vishidinc = Matrix.zerosf(numdims, numHiddenUnits)
    var hidbiasinc = Vector.zerosf(numHiddenUnits)
    var visbiasinc = Vector.zerosf(numdims)

    tic()
    var epoch = 0
    while (epoch < maxEpoch) {
      var errsum = 0f
      var batch = 0
      while (batch < numbatches) {
        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
        val poshidprobs = (data * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
        val posprods = data.t * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol
        val poshidstates = (poshidprobs :> Matrix.randf(numcases, numHiddenUnits))

        // Negative phase
        val negdata = (poshidstates * vishid.t + visbiases.replicate(numcases, 1)).sigmoidf
        val neghidprobs = (negdata * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
        val negprods = negdata.t * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol
        val diff = data - negdata
        errsum += (diff *:* diff).sum

        // Update weights and biases
        val momentum = if (epoch > 5) finalmomentum else initialmomentum
        vishidinc = vishidinc * momentum + ((posprods - negprods) / numcases  - (vishid * weightcost))*epsilonw
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid += vishidinc
        visbiases += visbiasinc
        hidbiases += hidbiasinc
        batch += 1
      }
      println("--> Epoch " + epoch)
      println(" error = " + errsum)
      epoch += 1
    }
    toc()

  }
}
