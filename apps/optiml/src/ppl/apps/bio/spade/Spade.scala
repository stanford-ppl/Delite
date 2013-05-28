package ppl.apps.bio.spade

import ppl.dsl.optiml._ 

object SpadeRunner extends OptiMLApplicationRunner with Spade

trait Spade extends OptiMLApplication with Downsampling with Upsampling with Clustering {
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
    println("")

    /********* clustering *********/
    /*
    val data = readMatrix(args(0) + "/data.txt")
    tic()
    val assgn = cluster(data, 200)
    toc(assgn)
    for(i <- 0 to 9)
      print(assgn(i)+" ")
    println("")
    */
    
    /*
    def next(numbers: Rep[DenseVector[Double]], i: Int):Rep[DenseVector[Double]] =
    if(i == 5) numbers
    else {
      val new_numbers = numbers.partition(_ > 0.5)._1
      var j = 0
      while(j < new_numbers.length){
        new_numbers(j) = new_numbers(j) - 0.1
        j += 1
      }
      new_numbers.pprint
      next(new_numbers, i+1)
    }
    next(numbers, 0)
    */
    
    /********* upsampling *********/
    /*
    val tbl = readMatrix(args(0) + "/tbl.txt")
    val cluster_data = readMatrix(args(0) + "/cluster_data.txt")
    val cluster_assign = readMatrix(args(0) + "/cluster_assign.txt").mapRows{row:Rep[DenseVector[Double]]=> row(0).AsInstanceOf[Int]}

    tic()
    val assign = upsample(tbl, cluster_data, cluster_assign)
    toc(assign)

    for(i <- 0 to 9)
      print(assign(i)+" ")
    println("")
    */
  }

}
