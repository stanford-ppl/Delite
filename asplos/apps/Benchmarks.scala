import asplos._

object PPLBenchmarks extends DeliteDSLBenchmarks {
  def main(args: Array[String]) { 
    //runBenchmark( GDALiteCompiler ) 
    stageBenchmark( CollectTestLiteCompiler )
  }
}

object SMALBenchmarks extends DeliteDSLBenchmarks {
  def main(args: Array[String]) { 
    runBenchmark( GDACompiler ) 
  }
}
