package ppl.dsl.tests.GPU_tests

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object RBMLoopRunner extends OptiMLApplicationRunner with RBMLoop

trait RBMLoop extends OptiMLApplication {

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

    tic()
    //for (epoch <- 0 until maxEpoch) {
      val epoch = 0
      var errsum = 0f
      //for (batch <- 0 until numbatches) {
	    var batch = 0
        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        //PerformanceTimer.start("RBM-posphase", false)
        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
		//println("data is "); data.pprint
        val poshidprobs = (data * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
		//println("poshidpobs is "); poshidprobs.pprint
        val posprods = data.t * poshidprobs
		//println("posprods is "); posprods.pprint
        val poshidact = poshidprobs.sumCol
		//println("poshidact is ");poshidact.pprint
        val posvisact = data.sumCol
		//println("posvisact is "); posvisact.pprint
        val poshidstates = (poshidprobs :> Matrix.randf(numcases, numHiddenUnits))
		//println("poshidstates is "); poshidstates.pprint
        //val poshidstates = (poshidprobs zip Matrix.randf(numcases, numHiddenUnits)){ (a,b) => if (a < b) 0f else 1f }
        //PerformanceTimer.stop("RBM-posphase", false)

        // Negative phase
        //PerformanceTimer.start("RBM-negphase", false)
        val negdata = (poshidstates * vishid.t + visbiases.replicate(numcases, 1)).sigmoidf
		//println("negdata is "); negdata.pprint
        val neghidprobs = (negdata * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
		//println("neghidprobs is "); neghidprobs.pprint
        val negprods = negdata.t * neghidprobs
		//println("negprods is "); negprods.pprint
        val neghidact = neghidprobs.sumCol
		//println("neghidact is "); neghidact.pprint
        val negvisact = negdata.sumCol
		//println("negvisact is "); negvisact.pprint
        val diff = data - negdata
		//println("diff is "); diff.pprint
        errsum += (diff *:* diff).sum
        //PerformanceTimer.stop("RBM-negphase", false)

        // Update weights and biases
        //PerformanceTimer.start("RBM-biasupdates", false)
        val momentum = if (epoch > 5) finalmomentum else initialmomentum
        vishidinc = vishidinc * momentum + ((posprods - negprods) / numcases  - (vishid * weightcost))*epsilonw
        visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (unit(epsilonvb) / numcases) // TODO aks: why is unit needed here?
        hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (unit(epsilonhb) / numcases)

        vishid = vishid + vishidinc
        visbiases = visbiases + visbiasinc
        hidbiases = hidbiases + hidbiasinc
        //PerformanceTimer.stop("RBM-biasupdates", false)
      //}
      println("--> Epoch " + epoch)
      println(" error = " + errsum)
    //}
    toc()

    //PerformanceTimer.print("RBM-posphase")
    //PerformanceTimer.save("RBM-posphase")
    //PerformanceTimer.print("RBM-negphase")
    //PerformanceTimer.save("RBM-negphase")
    //PerformanceTimer.print("RBM-biasupdates")
    //PerformanceTimer.save("RBM-biasupdates")
  }
}
