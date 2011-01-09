import scalaBLAS._

object Test {
	def main(args:Array[String]) {
		val in1_numRows = 12
		val in1_numCols = 20
		val in2_numRows = 20
		val in2_numCols = 17

		val in1 = new Array[Double](in1_numRows*in1_numCols)
		val in2 = new Array[Double](in2_numRows*in2_numCols)
		val out = new Array[Double](in1_numRows*in2_numCols)
		val out_correct = new Array[Double](in1_numRows*in2_numCols)

		/* Initialize */
		for(i <- 0 until in1_numRows*in1_numCols) {
			in1(i) = i*1.0
		}
		for(i <- 0 until in2_numRows*in2_numCols) {
			in2(i) = i*1.0
		}
		for(i <- 0 until in1_numRows*in2_numCols) {
			out(i) = 0.0 
			out_correct(i) = 0.0
		}

		/* Call MKL BLAS library */
		matMult(in1,in2,out,in1_numRows,in1_numCols,in2_numCols)

		/* Print the result */
		for(i <- 0 until in1_numRows) {
			for(j <- 0 until in2_numCols) {
				print(" " + out(i*in2_numCols+j))
			}
			println("")
		}

		/* Naive implementation */
		for(i <- 0 until in1_numRows) {
			for(j <- 0 until in2_numCols) {
				var temp = 0.0
				for(k <- 0 until in1_numCols) {
					temp += in1(in1_numCols*i+k)*in2(in2_numCols*k+j)
				}
				out_correct(in2_numCols*i+j) = temp
			}
		}

		/* Compare */
		for(i <- 0 until in1_numRows*in2_numCols) {
			if(out(i) != out_correct(i)) println("ERROR: " + out(i) + "," + out_correct(i))
		}

		println("MatMult Test done")

	}
}

