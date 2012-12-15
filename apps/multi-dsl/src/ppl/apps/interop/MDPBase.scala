package ppl.apps.interop


object MDPModelBase {

  def printUsage = {
    println("MDP <#states> <#actions> <#maxIter> <version>")
    exit(-1)
  }

  def main(args: Array[String]) {
    if (args.length < 4) printUsage

    val size = args(0).toInt
    val numActions = args(1).toInt
    val maxIter = args(2).toInt
    val version = args(3)

    val discountFactor = 0.9
    val tolerance = 1e-3
    if (version == "lib") {
      var actionResults = Map.empty[Action, (Matrix,Vector)]
      for (i <- 0 until numActions) {
        val cost = Vector.rand(size)
        val P = Matrix.rand(size,size)
        actionResults += Pair(i, (P,cost))
      }
      val initValue = Vector.rand(size)

      val start = System.currentTimeMillis
      val (value, actions) = valueIteration2(actionResults, initValue, discountFactor, tolerance, maxIter)
      val stop = System.currentTimeMillis
      //value.pprint
      println("time elapsed: " + (stop-start)/1e3)
    }
    else {
      val cost = new Array[Array[Double]](numActions)
      val prob = new Array[Array[Double]](numActions)
      val actions = new Array[Double](numActions)
      for (i <- 0 until numActions) {
        actions(i) = i
        cost(i) = Vector.rand(size).data
        prob(i) = Matrix.rand(size,size).data
      }
      val initValue = Vector.rand(size).data

      val start = System.currentTimeMillis
      val (value, act) = valueIterationOpt(actions, prob, cost, initValue, discountFactor, tolerance, maxIter)
      val stop = System.currentTimeMillis
      println("time elapsed: " + (stop-start)/1e3)
    }
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

  def valueIteration2(actionResults: Map[Double, (Matrix, Vector)], initValue: Vector, discountFactor: Double, tolerance: Double, maxIter: Int) = {
    var optimalActions: Vector = null
    
    var delta = Double.MaxValue
    var value = initValue
    var iter = 0
    var time = 0L
    while (Math.abs(delta) > tolerance && iter < maxIter) {
      val allValues = actionResults map { case (action, (prob, cost)) => (action, Vector.fill(0,value.length)(i => (prob(i) * value(i) * discountFactor + cost(i)).sum)) }
      val newValue = allValues.map(_._2) reduce { (v1,v2) => Vector.fill(0,value.length){ i => if (v1(i) <= v2(i)) v1(i) else v2(i) } }
      optimalActions = allValues.map(_._2).reduce { (v1,v2) => Vector.fill(0,value.length){ i => if (v1(i) <= v2(i)) 0.0 else 1.0 } }
      
      iter += 1
      delta = diff(newValue, value)
      value = newValue
    }
    println("iters " + iter)
    (value, optimalActions)
  }

  def valueIterationOpt(actions: Array[Double], prob: Array[Array[Double]], cost: Array[Array[Double]], initValue: Array[Double], discountFactor: Double, tolerance: Double, maxIter: Double) = {
    var optimalActions: Array[Double] = null

    val max = Double.MaxValue
    var delta = max
    var value = initValue
    var iter = 0
    val len = value.length
    while (Math.abs(delta) > tolerance && iter < maxIter) {
      val vMin = new Array[Double](len)
      var j = 0
      while (j < len) {
        vMin(j) = max
        j += 1
      }
      val actMin = new Array[Double](len)
      var a = 0
      while (a < actions.length) {
        val act = actions(a)
        var i = 0
        val vTemp = new Array[Double](len)
        val p = prob(a)
        val c = cost(a)
        while (i < len) {
          var vi = 0.0
          var k = 0
          while (k < len) {
            val offset = i*len
            vi += p(offset + k) * value(i) * discountFactor + c(i)
            k += 1
          }
          vTemp(i) = vi
          i += 1
        }
        i = 0
        while (i < len) {
          if (vMin(i) > vTemp(i)) {
            vMin(i) = vTemp(i)
            actMin(i) = act
          }
          i += 1
        }
        a += 1
      }

      optimalActions = actMin
      iter += 1
      delta = diff(vMin, value)
      value = vMin
    }
    println("iters " + iter)
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

  def diff(x: Array[Double], y: Array[Double]) = {
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
    val data = new Array[Double](length)

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

    def +(s: Double): Vector = {
      val res = new Vector(length)
      var i = 0
      while (i < length) {
        res(i) = this(i) + s
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

    def sum(): Double = {
      var acc = 0.0
      var i = 0
      while (i < length) {
        acc += this(i)
        i += 1
      }
      acc
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

  class VectorView(length: Int, offset: Int, _data: Array[Double]) extends Vector(length) {
    override val data = _data
    override def apply(n: Int) = data(offset + n)
    override def update(n: Int, x: Double) = data(offset + n) = x
  }

  class Matrix(val numRows: Int, val numCols: Int) {
    val data = new Array[Double](numRows * numCols)
    def apply(i: Int, j: Int) = data(i * numCols + j)
    def update(i: Int, j: Int, x: Double) = data(i * numCols + j) = x

    def apply(i: Int): Vector = new VectorView(numCols, i*numCols, data)

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
