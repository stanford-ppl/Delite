package ppl.apps.bio.spade

import ppl.dsl.optiml.{Vector,Matrix}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

object SpadeRunner extends OptiMLApplicationRunner with Spade

trait Spade extends OptiMLApplication with Downsampling {
  def print_usage = {
    println("Usage: Spade <input data file> <output data file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    /********* downsampling *********/

    val data = UnsupervisedTrainingSet(readMatrix(args(0)))
    tic()
    val densities = downsample(data, 5., 5., DenseVector[Int](0), false, 1.)
    toc(densities)
    var i = 0
    while (i < 9){
      print(densities(i) + " ")
      i += 1
    }
    println()
    /*
    if (MLOutputWriter.write(Matrix(densities),args(1))==false)
      println("Failed to write output to file")
    else
      println("Downsampling finished")
    */

    /********* clustering *********/
 /*
    val data = TrainingSet(MLInputReader.read(args(0) + "/data.txt"))
    tic()
    val assgn = Clustering.cluster(data, 200)
    toc(assgn)
    for(i <- 0 to 9)
      print(assgn(i)+" ")
    print("\n")
    */

    /********* upsampling *********/
    /*
    val tbl = MLInputReader.read(args(0) + "/tbl.txt")
    val cluster_data = MLInputReader.read(args(0) + "/cluster_data.txt")
    val cluster_assign = MLInputReader.read(args(0) + "/cluster_assign.txt").mapRowsToVec(_(0).toInt)

    tic
    val assign = Upsampling.assign_cluster(tbl, cluster_data, cluster_assign)
    toc(assign)

    for(i <- 0 to 9)
      print(assign(i)+" ")
    print("\n")
    */
  }

}
