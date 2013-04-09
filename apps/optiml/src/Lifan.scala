import ppl.dsl.optiml._

object LifanRunner extends OptiMLApplicationRunner with Lifan
trait Lifan extends OptiMLApplication { 

  def main() = {
    type Graph_rec = Record{
      val feat_file: String;
      val adj_file: String;
      val feat: DenseMatrix[Double];
      val adj_mat: DenseMatrix[Double];
      val n_node: Int;       
      // val sp_mat: DenseMatrix[Double]
    }
    
    def NewGraph(input: Rep[String], num: Rep[Int]) = {
      val _feat_file = input+num.ToString+".feat"
      val _adj_file = input+num.ToString+".adj"
      val _adj_mat = readMatrix(input+num.ToString+".adj",line=>line.toDouble)
      
      new Record {
        val feat_file = _feat_file
        val adj_file = _adj_file
        val feat = readMatrix(_feat_file,line=>line.toDouble)
        val adj_mat = _adj_mat
        val n_node = _adj_mat.numRows
        // val sp_mat = convert_to_sp(adj_mat,n_node)
      }         
    } 
    
    val n_graph = 10
    val graph = (0::n_graph){i=>NewGraph(".",i+1)}
    
    def foo(x: Rep[Graph_rec]) = x.n_node
    
      /*
      val v = readVector("vetex.dat")
      v.pprint

      // val m = readMatrix("adjacent.dat", s => s.toInt)
      val m = readMatrix("adjacent.dat")
      m.pprint

      val n_node=v.length
      println(n_node)

      val init = (0::n_node, 0::n_node) { (i,j) =>
        if (i == j) 0
        else if (m(i,j) != 0) m(i,j)
        else 999999
      }

      init.pprint
      val out = init.mutable
      var k = 0
      
      var out = init
      while (k<n_node) {   
        out = min(out, broadcastSum(out(k), out.getCol(k)))
        
        // optiml (parallel) for loops: writes must be disjoint   
        for (i <- 0::n_node) {
          for (j <- 0::n_node) {
            out(i,j) = min(out(i,j),out(i,k)+out(k,j))
          }
        }
        k += 1
      }
      out.pprint     
      */
      
      /*
      val v = (Vector.rand(10)*10).map(e => e.AsInstanceOf[Int]) //readVector("vetex.dat")
      // v.pprint
    
      val m = (Matrix.rand(10,10)*10).map(e => e.AsInstanceOf[Int]).mutable // readMatrix("adjacent.dat")
      // m.pprint

      val n_node=v.length
      println(n_node)
      
      
      val init = (0::n_node, 0::n_node) { (i,j) =>
        if (i == j) 0
        else if (m(i,j) != 0) m(i,j)
        else 999999
      }

      init.pprint
      val out = init.mutable
      var k = 0
      while (k<n_node) {    
        // optiml (parallel) for loops: writes must be disjoint    
        for (i <- 0::n_node) {
          for (j <- 0::n_node) {
            out(i,j) = min(out(i,j),out(i,k)+out(k,j))
          }
        }
        k += 1
      }
      out.pprint      
      */
      
      /*
      // TODO: couldn't find following op error when using:
      //       
      var k = 0
      while (k<n_node) {  
        
        // foo
        val z = out
        out = (0::n_node, 0::n_node) { (i,j) =>
          min(out(i,j),out(i,k)+out(k,j))
        }  
        // bar
        out = baz
        
        k += 1
      }
      */
      
      // doesn't work because we don't change out each time:
      /*
      val mins = (0::n_node, 0::n_node) { (i,j) =>
        ((0::n_node) { k => min(out(i,j), out(i,k)+out(k,j)) }).min
      }      
      mins.pprint
      */
      
      /*
      println("0: ")
      m.pprint
      
      var i = 0
      var j = 0
      while(i < n_node) {
        while(j < n_node) {
          if (i == j) {
            m(i,j) = 0
          }
          else{
            if (m(i,j) == 0) {
              m(i,j) = 999999
            }
          }
          j += 1
        } 
        j = 0
        i += 1
      }
      
      println("1:" )
      m.pprint
      
      var k = 0
      i = 0
      j = 0
      while(k<n_node){
        while(i<n_node){
          while(j<n_node){
            m(i,j)=min(m(i,j),m(i,k)+m(k,j))
            j += 1
          }
          j = 0
          i += 1
        }
        i = 0
        k += 1
      }
      
      println("2: ")      
      m.pprint   
    */         
  }
}