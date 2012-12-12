package ppl.apps.interop


object MDPModelBase {

  def printUsage = {
    println("MDP <#states> <#actions> <#maxIter>")
    exit(-1)
  }

  def main(args: Array[String]) {
    if (args.length < 3) printUsage

    val size = args(0).toInt
    val numActions = args(1).toInt
    val maxIter = args(2).toInt

    val cost = Vector.rand(size)
    val P = Matrix.rand(size,size)

    var actionResults = Map.empty[Action, (Matrix,Vector)]
    for (i <- 0 until numActions) {
      actionResults += Pair(i, (P,cost))
    }

    val initValue = Vector.rand(size)
    val discountFactor = 0.9
    val tolerance = 1e-3

    val start = System.currentTimeMillis
    val (value, actions) = valueIteration(actionResults, initValue, discountFactor, tolerance, maxIter)
    val stop = System.currentTimeMillis

    value.pprint
    println("time elapsed: " + (stop-start)/1e3)
  }

  type Action = Double

  //infinite horizon value iteration for a Markov Decision Process
  def valueIteration(actionResults: Map[Action, (Matrix,Vector)], initValue: Vector, discountFactor: Double, tolerance: Double = 1e-3, maxIter: Int = 1000) = {
    var optimalActions: Vector = null
    
    var delta = Double.MaxValue
    var value = initValue
    var iter = 0
    var time = 0L
    while (Math.abs(delta) > tolerance && iter < maxIter) {
      //check the value for each action at each state
      val allValues = actionResults map { case (action, (prob, cost)) => 
        val startTime = System.currentTimeMillis
        val v = prob*value
        time += System.currentTimeMillis - startTime
        (action, v * discountFactor + cost)
      }
      //choose the best action per state and the corresponding value
      val newValue = allValues.values reduce { (v1,v2) => Vector.fill(0,v1.length){ i => if (v1(i) <= v2(i)) v1(i) else v2(i) } }
      optimalActions = allValues.values reduce { (v1,v2) => Vector.fill(0,v1.length){ i => if (v1(i) <= v2(i)) 0.0 else 1.0 } }
      
      iter += 1
      delta = diff(newValue, value)
      value = newValue
    }
    println("mult time: " + time/1e3)

    (value, optimalActions)
  }

  def diff(x: Vector, y: Vector) = {
    var acc = 0.0
    var i = 0
    while (i < x.length) {
      acc += Math.abs(x(i) - y(i))
      i += 1
    }
    acc
  }

  object Vector {
    def fill[T](start: Int, end: Int)(f: Int => Double): Vector = {
      val res = new Vector(end-start)
      var i = 0
      var j = start
      while (j < end) {
        res(i) = f(j)
        i += 1
        j += 1
      }
      res
    }

    def rand(n: Int): Vector = {
      val r = new java.util.Random
      val res = new Vector(n)
      var i = 0
      while (i < n) {
        res(i) = r.nextDouble
        i += 1
      }
      res
    }
  }

  class Vector(val length: Int) {
    private val data = new Array[Double](length)

    def apply(n: Int) = data(n)
    def update(n: Int, x: Double) = data(n) = x
    
    def +(that: Vector): Vector = {
      val res = new Vector(length)
      var i = 0
      while (i < length) {
        res(i) = this(i) + that(i)
        i += 1
      }
      res
    }

    def *(s: Double): Vector = {
      val res = new Vector(length)
      var i = 0
      while (i < length) {
        res(i) = this(i) * s
        i += 1
      }
      res
    }

    def pprint {
      var i = 0
      print("[ ")
      while (i < length) {
        print(this(i))
        print(" ")
        i += 1
      }
      println("]")
    }

  }

  object Matrix {
    def rand(n: Int, m: Int): Matrix = {
      val r = new java.util.Random
      val res = new Matrix(n,m)
      var i = 0
      while (i < n*m) {
        res.data(i) = r.nextDouble
        i += 1
      }
      res
    }
  }

  class Matrix(val numRows: Int, val numCols: Int) {
    private val data = new Array[Double](numRows * numCols)
    def apply(i: Int, j: Int) = data(i * numCols + j)
    def update(i: Int, j: Int, x: Double) = data(i * numCols + j) = x

    def *(v: Vector): Vector = {
      val res = new Vector(numRows)
      var i = 0
      while (i < numRows) {
        var j = 0
        var acc = 0.0
        while (j < numCols) {
          acc += this(i,j) * v(j)
          j += 1
        }
        res(i) = acc
        i += 1
      }
      res
    }
  }

}
