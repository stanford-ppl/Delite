import asplos._

// Sets of tests to run at the same time (fill these in as needed)
object PPLBenchmarks extends DeliteDSLBenchmarks {
  def main(args: Array[String]) { 
    stageBenchmark( MatmultBlocked )
  }
}
