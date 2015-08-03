import asplos._ 

trait TPCHQ6Frame extends PPLApp {
  val minDate = (1994 << 9) + (1 << 5) + 1 // 1020961
  val maxDate = (1995 << 9) + (1 << 5) + 1 // 1021473

  def query(dates: Rep[Array1D[Int]], quants: Rep[Array1D[Int]], discounts: Rep[Array1D[Float]], prices: Rep[Array1D[Float]], N: Rep[Int]): Rep[Float]
  def main() {
    val shipdates = read(DATA_FOLDER + "tpch/dates.dat").map{_.toInt}
    val discounts = read(DATA_FOLDER + "tpch/discounts.dat").map{_.toFloat}
    val quantities = read(DATA_FOLDER + "tpch/quantities.dat").map{_.toInt}
    val prices = read(DATA_FOLDER + "tpch/prices.dat").map{_.toFloat}
    val N = shipdates.length

    val revenue = query(shipdates, quantities, discounts, prices, N)
    println(revenue)
  }
}

object TPCHQ6 extends PPLCompiler with TPCHQ6App
object TPCHQ6Func extends PPLCompiler with TPCHQ6App {
  registerFunction(query _)
  override def functionName = "query"
}
trait TPCHQ6App extends TPCHQ6Frame {
  def query(dates: Rep[Array1D[Int]], quants: Rep[Array1D[Int]], discounts: Rep[Array1D[Float]], prices: Rep[Array1D[Float]], N: Rep[Int]): Rep[Float] = {
    // ---------- Tiling Hints -----------
    tile(N, tileSize = 200, max = ?)
    // -----------------------------------

    filterReduce(N)(0.0f){i => 
      dates(i) > minDate && dates(i) < maxDate && 
      discounts(i) >= 0.05f && discounts(i) <= 0.07f && quants(i) < 24
    }{i => prices(i) * discounts(i) }{_+_}
  }
}

object TPCHQ6Blocked extends PPLCompiler with TPCHQ6BlockedApp
object TPCHQ6BlockedFunc extends PPLCompiler with TPCHQ6BlockedApp {
  registerFunction(query _)
  override def functionName = "query"
}
trait TPCHQ6BlockedApp extends TPCHQ6Frame {
  def query(dates: Rep[Array1D[Int]], quants: Rep[Array1D[Int]], discounts: Rep[Array1D[Float]], prices: Rep[Array1D[Float]], N: Rep[Int]): Rep[Float] = {
    // ---------- Tiling Hints -----------
    tile(N, tileSize = 200, max = ?)
    // -----------------------------------

    tiledReduce(N)(0.0f){ii =>
      val datesBlk = dates.bslice(ii)
      val quantsBlk = quants.bslice(ii)
      val discountsBlk = discounts.bslice(ii)
      val pricesBlk = prices.bslice(ii)
      filterReduce(ii.len)(0.0f){i => 
        datesBlk(i) > minDate && datesBlk(i) < maxDate && 
        discountsBlk(i) >= 0.05f && discountsBlk(i) <= 0.07f && quantsBlk(i) < 24
      }{i => pricesBlk(i) * discountsBlk(i) }{_+_}
    }{_+_}
    
  }
}

