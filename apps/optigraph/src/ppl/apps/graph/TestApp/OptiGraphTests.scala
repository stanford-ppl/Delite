package ppl.apps.graph.TestApp

import ppl.dsl.optigraph._
import ppl.delite.framework.DeliteApplication

object GraphAppRunner extends OptiGraphApplicationRunner with OptiGraphTests

/*
 *  -----------------------------------------
 *  OptiGraph tests / sample applications
 *  -----------------------------------------
*/

trait OptiGraphTests extends OptiGraphApplication {
  
  def test_graphOps() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val e1 = g.AddEdge(n1, n2)
    // repeated edge
    val e2 = g.AddEdge(n1, n2)
    val e3 = g.AddEdge(n2, n1)
    // self-edge
    val e4 = g.AddEdge(n1, n1)
    g.Freeze
    
    println("Test Graph")
    if(g.NumNodes != 2) {
      println("[FAIL] Wrong number of nodes. Expected value = 2, Actual value " + g.NumNodes)
    } else {
      println("[OK] Number of nodes is correct")
    }
    
    if(g.NumEdges != 4) {
      println("[FAIL] Wrong number of edges. Expected value = 4, Actual value " + g.NumNodes)
    } else {
      println("[OK] Number of edges is correct")
    }
    
    if(g.Node(n1.Id).Id != n1.Id) {
      println("[FAIL] Wrong node returned")
    } else {
      println("[OK] Correct node returned")
    }
    if(g.Edge(e1.Id).Id != e1.Id) {
      println("[FAIL] Wrong edge returned")
    } else {
      println("[OK] Correct edge returned")
    }
    
    val nodes = g.Nodes.toSet
    val edges = g.Edges.toSet
    if(nodes.Size != 2 || edges.Size != 4) {
      println("[FAIL] Wrong nodes/edges collection size")
    } else {
      println("[OK] Correct nodes/edges collection size")
    }
    if((!nodes.Has(n1)) || (!nodes.Has(n2)) || (!edges.Has(e1)) || (!edges.Has(e2))
        || (!edges.Has(e3)) || (!edges.Has(e4))) {
      println("[FAIL] Wrong nodes/edges collection contents")
    } else {
      println("[OK] Correct nodes/edges collection contents")
    }
    
    //---------//
    
    val g2  = DGraph()
    val n3 = g2.AddNode
    val n4 = g2.AddNode
    val e5 = g2.AddEdge(n3, n4)
    val e6 = g2.AddEdge(n3, n4)
    val e7 = g2.AddEdge(n4, n3)
    val e8 = g2.AddEdge(n3, n3)
    val g_s = g2.Snapshot
    
    if(g_s.NumNodes != g2.NumNodes || g_s.NumEdges != g2.NumEdges) {
      println("[FAIL] Wrong snapshot graph size")
    } else {
      println("[OK] Correct snapshot graph size")
    }
    
    if(!((g_s.Node(0).Degree == 3 || g_s.Node(1).Degree == 3) && 
        (g_s.Node(0).Degree == 1 || g_s.Node(1).Degree == 1))) {
      println("[FAIL] Wrong snapshot graph node connections")
    } else {
      println("[OK] Correct snapshot graph node connections")
    }
    
    val g3 = UGraph()
    val n5 = g3.AddNode
    val n6 = g3.AddNode
    val e9 = g3.AddEdge(n5, n6)
    g3.Freeze
    if((n5.OutDegree != 1) && (n6.OutDegree != 1) && (n5.InDegree != 1) && (n6.InDegree != 1)) {
      println("[FAIL] Undirected graph wrong edge connectivity")
    } else {
      println("[OK] Undirected graph connectivity is correct")
    }
    
  }
  
  def test_deferrable() {
    println("Test Deferrable")
    val d = Deferrable[Double](1.0)
    if(d.value != 1.0) {
      println("[FAIL] Expected value = 1.0, Actual value = " + d.value)
    } else {
      println("[OK] Current value is correct")
    }
    d <= 2.0
    if(d.value != 1.0) {
      println("[FAIL] After deferral, expected value = 1.0, Actual value = " + d.value)
    } else {
      println("[OK] Deferral did not affect current value")
    }
    d.assign()
    if(d.value != 2.0) {
      println("[FAIL] After assignment, expected value = 2.0, Actual value = " + d.value)
    } else {
      println("[OK] Current value is correct after assignment")
    }
   
    d.setValue(1.0)
    d.assign()
    if(d.value != 1.0) {
      println("[FAIL] After repeated assignment, expected value = 1.0, Actual value = " + d.value)
    } else {
      println("[OK] Repeated assignment did not affect value")
    }
  }
  
  def test_reduceable() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val n3 = g.AddNode
    val n4 = g.AddNode
    g.Freeze
    
    println("Test Reduction Assignments: SUM ")
    val sum = Reduceable[Int](5)
    for(n <- g.Nodes) {
      sum += 1
    }
    if(sum.value != 9) {
      println("[FAIL] Expected value = 9, Actual value = " + sum.value)
    } else {
      println("[OK] Sum is correct.")
    }
   
    //-------//
    
    println("Test Reduction Assignments: PROD ")
    val prod = Reduceable[Int](2)
    for(n <- g.Nodes) {
      prod *= 2
    }
    if(prod.value != 32) {
      println("[FAIL] Expected value = 32, Actual value = " + prod.value)
    } else {
      println("[OK] Product is correct.")
    }
    
    //-------//
    
    println("Test Reduction Assignments: ALL ")
    val all = Reduceable[Boolean](true)
    val all2 = Reduceable[Boolean](true)
    for(n <- g.Nodes) {
      all &&= false
      all2 &&= true
    }
    if(all.value != false) {
      println("[FAIL] Expected value = false, Actual value = " + all.value)
    } else if(all2.value != true) {
      println("[FAIL] Expected value = true, Actual value = " + all2.value)
    } else {
      println("[OK] ALL is correct.")
    }
    
    //-------//
    
    println("Test Reduction Assignments: ANY ")
    val any = Reduceable[Boolean](false)
    val any2 = Reduceable[Boolean](true)
    for(n <- g.Nodes) {
      any ||= true
      any2 ||= false
    }
    if(any.value != true) {
      println("[FAIL] Expected value = true, Actual value = " + any.value)
    } else if(any2.value != true) {
      println("[FAIL] Expected value = true, Actual value = " + any2.value)
    } else {
      println("[OK] ANY is correct.")
    }
    
    //-------//
    
    println("Test Reduction Assignments: COUNT ")
    val count = Reduceable[Int](1)
    val count2 = Reduceable[Int](0)
    for(n <- g.Nodes) {
      count ++= true
      count2 ++= false
    }
    if(count.value != 5) {
      println("[FAIL] Expected value = 4, Actual value = " + count.value)
    } else if(count2.value != 0) {
      println("[FAIL] Expected value = 0, Actual value = " + count2.value)
    } else {
      println("[OK] COUNT is correct.")
    }
    
    //-------//
    
    println("Test Reduction Assignments: MAX ")
    val max = Reduceable[Int](MIN_INT)
    for(n <- g.Nodes) {
      max >= n.Id
    }
    if(max.value != 3) {
      println("[FAIL] Expected value = 3, Actual value = " + max.value)
    } else {
      println("[OK] MAX is correct.")
    }
    
    //-------//
    
    println("Test Reduction Assignments: MIN ")
    val min = Reduceable[Int](MAX_INT)
    for(n <- g.Nodes) {
      min <= 1
    }
    if(min.value != 1) {
      println("[FAIL] Expected value = 1, Actual value = " + min.value)
    } else {
      println("[OK] MIN is correct.")
    }
    
    //-------//
    
    println("Test Reduceable Basic Ops")
    val red = Reduceable[Double](1.0)
    if(red.value != 1.0) {
      println("[FAIL] Expected value = 1.0, Actual value = " + red.value)
    } else {
      println("[OK] Got correct current value.")
    }
    red.setValue(2.0)
    if(red.value != 2.0) {
      println("[FAIL] Expected value = 2.0, Actual value = " + red.value)
    } else {
      println("[OK] Value was set correctly.")
    }
  }
  
  def test_reductions() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val n3 = g.AddNode
    val n4 = g.AddNode
    g.Freeze
    val np1 = NodeProperty[Int](g, 1)
    val np2 = NodeProperty[Boolean](g, true)
    val ns = NodeSet()
    
    // check empty/non-empty collections, with/without filters
    
    println("Test Reduction Expressions: SUM")
    val r1 = Sum(g.Nodes){np1(_)}
    val r2 = Sum(g.Nodes, (n: Rep[Node]) => (n.Id == 1)){np1(_)}
    val r3 = Sum(ns.Items){np1(_)}
    
    if(r1 != 4) {
      println("[FAIL] Expected value = 4, Actual value = " + r1)
    } else {
      println("[OK] Sum is correct.")
    }
    if(r2 != 1) {
      println("[FAIL] Expected value = 1, Actual value = " + r2)
    } else {
      println("[OK] Sum is correct.")
    }
    if(r3 != 0) {
      println("[FAIL] Expected value = 0, Actual value = " + r3)
    } else {
      println("[OK] Sum is correct.")
    }
    
    //-------//
    
    println("Test Reduction Expressions: PRODUCT")
    r1 = Product(g.Nodes){np1(_)}
    r2 = Product(g.Nodes, (n: Rep[Node]) => (n.Id == 1)){ n => unit(5)}
    r3 = Product(ns.Items){np1(_)}
    
    if(r1 != 1) {
      println("[FAIL] Expected value = 1, Actual value = " + r1)
    } else {
      println("[OK] Product is correct.")
    }
    if(r2 != 5) {
      println("[FAIL] Expected value = 5, Actual value = " + r2)
    } else {
      println("[OK] Product is correct.")
    }
    if(r3 != 0) {
      println("[FAIL] Expected value = 0, Actual value = " + r3)
    } else {
      println("[OK] Product is correct.")
    }
    
    //-------//
    
    println("Test Reduction Expressions: COUNT")
    r1 = Count(g.Nodes){(n: Rep[Node]) => (n.Id != 1)}
    r2 = Count(g.Nodes) {(n: Rep[Node]) => (n.Id == 1)}
    r3 = Count(ns.Items){(n: Rep[Node]) => (n.Id == 1)}
    
    if(r1 != 3) {
      println("[FAIL] Expected value = 3, Actual value = " + r1)
    } else {
      println("[OK] Count is correct.")
    }
    if(r2 != 1) {
      println("[FAIL] Expected value = 1, Actual value = " + r2)
    } else {
      println("[OK] Count is correct.")
    }
    if(r3 != 0) {
      println("[FAIL] Expected value = 0, Actual value = " + r3)
    } else {
      println("[OK] Count is correct.")
    }
    
    //-------//
    
    println("Test Reduction Expressions: MIN")
    r1 = Min(g.Nodes){(n: Rep[Node]) => n.Id}
    r2 = Min(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => n.Id}
    r3 = Min(ns.Items){np1(_)}
    
    if(r1 != 0) {
      println("[FAIL] Expected value = 0, Actual value = " + r1)
    } else {
      println("[OK] Min is correct.")
    }
    if(r2 != 1) {
      println("[FAIL] Expected value = 1, Actual value = " + r2)
    } else {
      println("[OK] Min is correct.")
    }
    if(r3 != MAX_INT) {
      println("[FAIL] Expected value = MAX_INT, Actual value = " + r3)
    } else {
      println("[OK] Min is correct.")
    }
    
    //-------//
    
    println("Test Reduction Expressions: MAX")
    r1 = Max(g.Nodes){(n: Rep[Node]) => n.Id}
    r2 = Max(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => n.Id}
    r3 = Max(ns.Items){np1(_)}
    
    if(r1 != 3) {
      println("[FAIL] Expected value = 3, Actual value = " + r1)
    } else {
      println("[OK] Max is correct.")
    }
    if(r2 != 1) {
      println("[FAIL] Expected value = 1, Actual value = " + r2)
    } else {
      println("[OK] Max is correct.")
    }
    if(r3 != MIN_INT) {
      println("[FAIL] Expected value = MIN_INT, Actual value = " + r3)
    } else {
      println("[OK] Max is correct.")
    }
    
    //-------//
    
    println("Test Reduction Expressions: ALL")
    val r1b = All(g.Nodes){(n: Rep[Node]) => (n.Id < 5)}
    val r2b = All(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => (n.Id == 2)}
    val r3b = All(ns.Items){(n: Rep[Node]) => (n.Id < 5)}
    
    if(r1b != true) {
      println("[FAIL] Expected value = true, Actual value = " + r1b)
    } else {
      println("[OK] All is correct.")
    }
    if(r2b != false) {
      println("[FAIL] Expected value = false, Actual value = " + r2b)
    } else {
      println("[OK] All is correct.")
    }
    if(r3b != true) {
      println("[FAIL] Expected value = true, Actual value = " + r3b)
    } else {
      println("[OK] All is correct.")
    }
    
    //-------//
    
    println("Test Reduction Expressions: ANY")
    r1b = Any(g.Nodes){(n: Rep[Node]) => (n.Id > 2)}
    r2b = Any(g.Nodes, (n: Rep[Node]) => (n.Id == 1)) {(n: Rep[Node]) => (n.Id == 2)}
    r3b = Any(ns.Items){(n: Rep[Node]) => (n.Id < 5)}
    
    if(r1b != true) {
      println("[FAIL] Expected value = true, Actual value = " + r1b)
    } else {
      println("[OK] Any is correct.")
    }
    if(r2b != false) {
      println("[FAIL] Expected value = false, Actual value = " + r2b)
    } else {
      println("[OK] Any is correct.")
    }
    if(r3b != false) {
      println("[FAIL] Expected value = false, Actual value = " + r3b)
    } else {
      println("[OK] Any is correct.")
    }
    
  }
  
  def test_traversals() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val n3 = g.AddNode
    val n4 = g.AddNode
    val n5 = g.AddNode
    val n6 = g.AddNode
    g.AddEdge(n1, n2)
    g.AddEdge(n1, n3)
    g.AddEdge(n2, n4)
    g.AddEdge(n2, n5)
    g.AddEdge(n4, n5)
    g.AddEdge(n4, n1)
    g.AddEdge(n4, n6)
    g.AddEdge(n6, n4)
    g.Freeze
    
    
    println("Test BFS")
    val nord = NodeSeq()
    val nordR = NodeSeq()
    val downNbrs = NodeSet()
    val upNbrs = NodeSet()
    
    InBFS(g, n1, { (n: Rep[Node]) => 
      nord.PushBack(n)
      println("nId = " + n.Id)
      if(n.Id == n4.Id) {
        downNbrs.AddSet(n.DownNbrs.toSet)
        upNbrs.AddSet(n.UpNbrs.toSet)
      }
    }, InReverse(n=>nordR.PushBack(n)))
    
    if(!((downNbrs.Size == 1) && (downNbrs.Has(n6)))) {
      println("[FAIL] DownNbrs set is incorrect")
    } else {
      println("[OK] DownNbrs set is correct")
    }
    
    if(!((upNbrs.Size == 1) && (upNbrs.Has(n2)))) {
      println("[FAIL] UpNbrs set is incorrect")
    } else {
      println("[OK] UpNbrs set is correct")
    }
    
    if(nord.Size != 6) {
      println("[FAIL] Number of nodes traversed is incorrect")
    } else {
      println("[OK] Number of nodes traversed is correct")
    }
    
    if(nord(5).Id != n6.Id || nord(0).Id != n1.Id) {
      println("[FAIL] Traversal order is incorrect")
    }
    
    if(nordR(5).Id != n1.Id || nordR(0).Id != n6.Id) {
      println("[FAIL] Reverse traversal order is incorrect")
    }
    // TODO: sort by Ids and test actual sequence
    
    // TODO: DFS
    println("Test DFS")
    val nord2 = NodeSeq()
    val nordR2 = NodeSeq()
    InDFS(g, n1, { (n: Rep[Node]) => 
      nord2.PushBack(n)
    }, InPost(n=>nordR2.PushBack(n)))
    
    if(nord2(0).Id != n1.Id) {
      println("[FAIL] Traversal order is incorrect")
    }
    
    if(nordR2(5).Id != n1.Id) {
      println("[FAIL] Reverse traversal order is incorrect")
    }
  }
  
  
  def test_nodeOpsEdgeOps() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val n3 = g.AddNode
    val n4 = g.AddNode
    val e1 = g.AddEdge(n1, n2)
    val e2 = g.AddEdge(n1, n3)
    val e3 = g.AddEdge(n2, n1)
    val e4 = g.AddEdge(n4, n1)
    val e5 = g.AddEdge(n3, n4)
    g.Freeze
    
    println("Test Node Out-Nbrs")
    val outNbrs = n1.OutNbrs.toSet
    if(outNbrs.Size != 2) {
      println("[FAIL] [Expected size = 2] [Actual size = " + outNbrs.Size+ "]")
    } else {
      println("[OK] Size correct.")
    }
    if(!(outNbrs.Has(n2) && outNbrs.Has(n3))) {
      println("[FAIL] Node missing.")
    } else {
      println("[OK] All nodes present.")
    }
    
    //-------//
    
    println("Test Node In-Nbrs")
    val inNbrs = n1.InNbrs.toSet
    if(inNbrs.Size != 2) {
      println("[FAIL] [Expected size = 2] [Actual size = " + inNbrs.Size+ "]")
    } else {
      println("[OK] Size correct.")
    }
    if(!(inNbrs.Has(n2) && inNbrs.Has(n4))) {
      println("[FAIL] Node missing.")
    } else {
      println("[OK] All nodes present.")
    }
    
    //-------//
    
    println("Test Node Out-Edges")
    val outEdges = n1.OutEdges.toSet
    if(outEdges.Size != 2) {
      println("[FAIL] [Expected size = 2] [Actual size = " + outEdges.Size+ "]")
    } else {
      println("[OK] Size correct.")
    }
    if(!(outEdges.Has(e1) && outEdges.Has(e2))) {
      println("[FAIL] Edge missing.")
    } else {
      println("[OK] All edges present.")
    }
    
    //-------//
    
    println("Test Node In-Edges")
    val inEdges = n1.InEdges.toSet
    if(inEdges.Size != 2) {
      println("[FAIL] [Expected size = 2] [Actual size = " + inEdges.Size+ "]")
    } else {
      println("[OK] Size correct.")
    }
    if(!(inEdges.Has(e3) && inEdges.Has(e4))) {
      println("[FAIL] Edge missing.")
    } else {
      println("[OK] All edges present.")
    }
    
    //-------//
    
    println("Test Node Out-Degree")
    val outDeg = n1.OutDegree
    if(outDeg != 2) {
      println("[FAIL] [Expected degree = 2] [Actual degree = " + outDeg + "]")
    } else {
      println("[OK] Degree correct.")
    }
    
    //-------//
    
    println("Test Node In-Degree")
    val inDeg = n1.InDegree
    if(inDeg != 2) {
      println("[FAIL] [Expected degree = 2] [Actual degree = " + inDeg + "]")
    } else {
      println("[OK] Degree correct.")
    }
    
    //-------//
    
    println("Test Edge From/To")
    if(e1.From.Id != n1.Id || e1.To.Id != n2.Id) {
      println("[FAIL] Edge To/From incorrectly set")
    } else {
      println("[OK] Edge To/From are correct.")
    }
    
  }
  
  def test_filters() {
    val G = rand_graph()
    val prop = NodeProperty[Int](G, 1)
    
    // 1. filter
    println("Test: Filter")
    val filteredNone = G.Nodes.filter(n => prop(n) == 1)
    println("[Expected size = " + G.NumNodes + "] [Filtered size = " + filteredNone.toSet.Size+ "]")
    val filteredAll = G.Nodes.filter(n => prop(n) == 2)
    println("[Expected size = 0] [Filtered size = " + filteredAll.toSet.Size + "]")
    
    //-------//
    
    // 2. sequential For with filter 
    println("Test: For with filter")
    For(G.Nodes, (n: Rep[Node]) => (prop(n) == 1)) { n => 
      println("[OK] n.Id = " + n.Id)
    }
    
    For(G.Nodes, (n: Rep[Node]) => (prop(n) == 2)) { n => 
      println("[FAIL] n.Id = " + n.Id)
    }
    
    //-------//
    
    // 3. parallel Foreach/foreach with filter
    println("Test: Foreach with filter")
    for(n <- G.Nodes if prop(n) == 1) {
      println("[OK] n.Id = " + n.Id)
    }
    
    for(n <- G.Nodes if prop(n) == 2) {
      println("[FAIL] n.Id = " + n.Id)
    }
    
    Foreach(G.Nodes, (n: Rep[Node]) => (prop(n) == 1)) { n => 
      println("[OK] n.Id = " + n.Id)
    }
    
    Foreach(G.Nodes, (n: Rep[Node]) => (prop(n) == 2)) { n => 
      println("[FAIL] n.Id = " + n.Id)
    }
    
    //-------//
    
    // 4. DFS with filter (i.e. navigator)
    println("Test: DFS with filter")
    InDFS(G, G.Node(0), (n:Rep[Node]) => prop(n) == 1, { (n: Rep[Node]) => 
      println("[OK] n.Id = " + n.Id)
    })
    InDFS(G, G.Node(0), (n:Rep[Node]) => prop(n) == 2, { (n: Rep[Node]) => 
      println("[FAIL] n.Id = " + n.Id)
    })
    
    //-------//
    
    // 5. BFS with filter (i.e. navigator)
    println("Test: BFS with filter")
    InBFS(G, G.Node(0), (n:Rep[Node]) => prop(n) == 1, { (n: Rep[Node]) => 
      println("[OK] n.Id = " + n.Id)
    })
    InBFS(G, G.Node(0), (n:Rep[Node]) => prop(n) == 2, { (n: Rep[Node]) => 
      println("[FAIL] n.Id = " + n.Id)
    })
  }
  
  def test_iterations() {   
    
    // empty / non-empty
    
    println("Test Parallel For-each")
    val set = NodeSet()
    for(n <- set.Items) {
      println("[FAIL] Iterable collection is empty")
    }
    Foreach(set.Items) { n =>
      println("[FAIL] Iterable collection is empty ")
    }

    val G  = Graph()
    val n1 = G.AddNode
    val n2 = G.AddNode
    G.Freeze
    val np = NodeProperty[Int](G, 0)
    
    val i = Reduceable[Int](0)
    for(n <- G.Nodes) {
      np(n) = 1
      i += 1
    }
    if(i.value != 2) {
      println("[FAIL] Expected number of iterations = 2, Actual number of iterations = " + i.value)
    } else {
      println("[OK] Num iterations is correct.")
    }
    if(np(n1) != 1 || np(n2) != 1) {
      println("[FAIL] Iteration block was not executed.")
    } else {
      println("[OK] Iteration block was executed.")
    }
    
    i.setValue(0)
    Foreach(G.Nodes) { n =>
      np(n) = 2
      i += 1
    }
    if(i.value != G.NumNodes) {
      println("[FAIL] Expected number of iterations = 2, Actual number of iterations = " + i.value)
    } else {
      println("[OK] Num iterations is correct.")
    }
    if(np(n1) != 2 || np(n2) != 2) {
      println("[FAIL] Iteration block was not executed.")
    } else {
      println("[OK] Iteration block was executed.")
    }
    
    //-------//
    
    println("Test Sequential For")
    For[Node, GIterable[Node]](G.Nodes) { (n: Rep[Node]) =>
      np(n) = 3
    }
    if(np(n1) != 3 || np(n2) != 3) {
      println("[FAIL] Iteration block was not executed.")
    } else {
      println("[OK] Iteration block was executed.")
    }
    
  }
  
  def test_nodeEdgeProps() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val e1 = g.AddEdge(n1, n2)
    val e2 = g.AddEdge(n2, n1)
    val e3 = g.AddEdge(n1, n1)
    g.Freeze
    
    println("Test NodeProperty")
    val np = NodeProperty[Int](g, 1)
    val np2 = NodeProperty[Int](g)
    // alias constructor 
    val np3 = NP[Int](g, 0)
    
    if(np(n1) != 1 || np(n2) != 1) {
      println("[FAIL] Expected value = 1, different actual value for node(s)")
    } else {
      println("[OK] Got correct value for all nodes ")
    }
    np(n1) = 2
    if(np(n1) != 2 ) {
      println("[FAIL] Updated expected value = 2, Actual value = " + np(n1))
    } else {
      println("[OK] Updated value correctly")
    }
    
    np.setAll(3)
    if(np(n1) != 3 || np(n2) != 3 ) {
      println("[FAIL] Set all expected value = 3, different actual value for node(s)")
    } else {
      println("[OK] Set all values correctly")
    }
    
    np <= (n2, 5)
    if(np(n2) != 3 ) {
      println("[FAIL] After deferral, expected value = 3, Actual value = " + np(n2))
    } else {
      println("[OK] Deferral did not affect current value")
    }
    
    np.assign(n2)
    if(np(n2) != 5) {
      println("[FAIL] After assignment, expected value = 5, Actual value = " + np(n2))
    } else {
      println("[OK] Assigned correct value")
    }
    
    np(n2) = 3
    np.assign(n2)
    if(np(n2) != 3) {
      println("[FAIL] After repeated assignment, expected value = 3, Actual value = " + np(n2))
    } else {
      println("[OK] Repeated assignment did not affect value")
    }
    
    np <= (n1, 5)
    np <= (n2, 5)
    if(np(n1) != 3 || np(n2) != 3) {
      println("[FAIL] After deferral, expected value = 3, different actual value for node(s)")
    } else {
      println("[OK] Deferral did not affect current values")
    }
    
    np.assignAll()
    if(np(n1) != 5 || np(n2) != 5) {
      println("[FAIL] After assignment, expected value = 5, different actual value for node(s)")
    } else {
      println("[OK] Assigned correct values")
    }
    
    np(n1) = 3
    np(n2) = 3
    np.assignAll()
    if(np(n1) != 3 || np(n2) != 3) {
      println("[FAIL] After repeated assignment, expected value = 3, different actual value for node(s)")
    } else {
      println("[OK] Repeated assignment did not affect values")
    }
    
    //---------//
    
    println("Test EdgeProperty")
    val ep = EdgeProperty[Int](g, 1)
    val ep2 = EdgeProperty[Int](g)
    // alias constructor 
    val ep3 = EP[Int](g, 0)
    
    if(ep(e1) != 1 || ep(e2) != 1 || ep(e3) != 1) {
      println("[FAIL] Expected value = 1, different actual value for edge(s)")
    } else {
      println("[OK] Got correct value for all edges ")
    }
    ep(e1) = 2
    if(ep(e1) != 2 ) {
      println("[FAIL] Updated expected value = 2, Actual value = " + ep(e1))
    } else {
      println("[OK] Updated value correctly")
    }
    
    ep.setAll(3)
    if(ep(e1) != 3 || ep(e2) != 3 || ep(e3) != 3) {
      println("[FAIL] Set all expected value = 3, different actual value for edge(s)")
    } else {
      println("[OK] Set all values correctly")
    }
    
    ep <= (e2, 5)
    if(ep(e2) != 3 ) {
      println("[FAIL] After deferral, expected value = 3, Actual value = " + ep(e2))
    } else {
      println("[OK] Deferral did not affect current value")
    }
    
    ep.assign(e2)
    if(ep(e2) != 5) {
      println("[FAIL] After assignment, expected value = 5, Actual value = " + ep(e2))
    } else {
      println("[OK] Assigned correct value")
    }
    
    ep(e2) = 3
    ep.assign(e2)
    if(ep(e2) != 3) {
      println("[FAIL] After repeated assignment, expected value = 3, Actual value = " + ep(e2))
    } else {
      println("[OK] Repeated assignment did not affect value")
    }
    
    ep <= (e1, 5)
    ep <= (e2, 5)
    ep <= (e3, 5)
    if(ep(e1) != 3 || ep(e2) != 3 || ep(e3) != 3) {
      println("[FAIL] After deferral, expected value = 3, different actual value for edge(s)")
    } else {
      println("[OK] Deferral did not affect current values")
    }
    
    ep.assignAll()
    if(ep(e1) != 5 || ep(e2) != 5 || ep(e3) != 5) {
      println("[FAIL] After assignment, expected value = 5, different actual value for edge(s)")
    } else {
      println("[OK] Assigned correct values")
    }
    
    ep(e1) = 3
    ep(e2) = 3
    ep(e3) = 3
    ep.assignAll()
    if(ep(e1) != 3 || ep(e2) != 3 || ep(e3) != 3) {
      println("[FAIL] After repeated assignment, expected value = 3, different actual value for edge(s)")
    } else {
      println("[OK] Repeated assignment did not affect values")
    }
    
  }
  
  def basic() {
    val g  : Rep[Graph] = null
    val g_empty : Rep[Graph] = null
    val n1 : Rep[Node] = null
    val n2 : Rep[Node] = null
    val n3 : Rep[Node] = null
    val n4 : Rep[Node] = null
    val e : Rep[Edge] = null
    val np : Rep[NodeProperty[Int]] = null
    val np2 : Rep[NodeProperty[String]] = null
    val np3 : Rep[NodeProperty[Boolean]] = null
    
    g = Graph()
    n1 = g.AddNode
    n2 = g.AddNode
    n3 = g.AddNode
    n4 = g.AddNode
    e = g.AddEdge(n1, n2)
    g.Freeze
    
    println("Graph construction finished")
    println("Num nodes: " + g.NumNodes)
    println("Num edges: " + g.NumEdges)
    println("Nodes: " + g.Nodes)
    println("Node 1: " + n1)
    println("Node 2: " + n2)
    println("Node 3: " + n3)
    println("Node 4: " + n4)
    println("Edges: " + g.Edges)
    println("Node 1 num neighbors: " + n1.NumNbrs)
    println("Node 2 num neighbors: " + n2.NumNbrs)

    println("FOREACH:")
    for(n <- g.Nodes) {
       println("Node: " + n)
    }
    Foreach(g.Nodes) { (n: Rep[Node]) =>println("Node in Foreach: " + n) }
    
    println("NODE PROP")
    np = NodeProperty[Int](g, 1)
    println("Created")
    println("n1.np = " + np(n1))
    println("n2.np = " + np(n2))
    np(n1) = unit(5)
    np(n2) = unit(6)
    println("n1.np = " + np(n1))

    np2 = NodeProperty[String](g, "a")
    np3 = NodeProperty[Boolean](g)
    val ep = EP[Int](g)
    
    println("n1.np2 = " + np2(n1))
    println("n2.np3 = " + np3(n2))
    np3.setAll(true)
    println("n1.np3 = " + np3(n1))
    println("n2.np3 = " + np3(n2))

    println("SUM = " + Sum(g.Nodes){np(_)})
    println("PROD = " + Product(g.Nodes){np(_)})
    println("MAX = " + Max(g.Nodes){np(_)})
    println("MIN = " + Min(g.Nodes){np(_)})
    println("COUNT = " + Count(g.Nodes){np(_) == 5})
    println("ALL = " + All(g.Nodes){np(_) == 5})
    println("ANY = " + Any(g.Nodes){np(_) == 5})
    
    println("NODE SET")
    val ns = NodeSet()
    ns.Add(n1)
    println("Node set size = " + ns.Size)
    println("Node set contains n1 = " + ns.Has(n1))

    println("IN BFS")
    InBFS(g, n1, n=>println("node prop: " + np(n)))
    
    println("NODE ORDER")
    val no = NodeOrder()
    no.PushFront(n1)
    no.PushFront(n2)
    no.PushFront(n3)
    no.PushFront(n4)
    
    val no2 = NodeOrder()
    no2.PushBack(n1)
    no2.PushBack(n2)
    no2.PushBack(n3)
    no2.PushBack(n4)
    
    println("Node order size = " + no.Size)
    println("Node order contains n1 = " + no.Has(n1))
    
    for(i <- no.Items) {
       println("Node: " + i)
    }
    
    for(i <- no2.Items) {
       println("Node: " + i)
    }
  }
  
  def rand_graph(): Rep[Graph] = {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val n3 = g.AddNode
    val n4 = g.AddNode
    val n5 = g.AddNode
    
    g.AddEdge(n1, n2)
    g.AddEdge(n1, n3)
    g.AddEdge(n2, n4)
    g.AddEdge(n3, n5)
    g.Snapshot
  }
  
  def page_rank()//G: Rep[Graph], e: Rep[Double], d: Rep[Double], max_iter: Rep[Int], 
      //PR: Rep[NodeProperty[Double]]) 
  {
    //val G = RandUniformGraph(5,5,1997L)
    val G = graph_load("/home/viq/delite/Delite/test.bin")
    
    val e = 0.001
    val d = 0.85
    val max_iter = 6
    val PR = NodeProperty[Double](G)
    
    val diff = Reduceable[Double](0.0)
    var cnt = 0
    val N = G.NumNodes.asInstanceOf[Rep[Double]]
    PR.setAll(1.0/N)

    println("G.N " + N)
    
    // move to ds
    val deg = NewArray[Int](G.NumNodes)
    for(t <- G.Nodes) {
      deg(t.Id) = t.OutDegree
    }
    
    tic(G, PR, deg)
    //var num_abs = 0
    //var v = 0.0
    var cond = true
    //val n = G.Node(0)
    while(cond) {     
      diff.setValue(0.0)
      for(t <- G.Nodes) {
        val Val: Rep[Double] = ((1.0 - d) / N) + d * Sum(t.InNbrs){
          w => PR(w) / deg(w.Id)//w.OutDegree
        }
        //val Val = v
        PR <= (t,Val)
        
        diff += Math.abs(Val - PR(t))
        //num_abs += 1
        //v += 1.0
      }
      PR.assignAll()
      cnt += 1
      cond = (diff.value > e) && (cnt < max_iter)
    }
    println("count = " + cnt)
    //println("abs times = " + num_abs)
    toc()
    
//    for(t <- G.Nodes) {
//      println(" PR " + t.Id + " " + PR(t))
//    }
  }
  
  def conductance() {
    //val G = rand_graph()
    val G = RandUniformGraph(100000,800000,1997L)
    
    val member = NodeProperty[Int](G)
    var i = 0
    for(n <- G.Nodes) {
      member(n) = i % 4
      //println("Node id = " + n.Id + " member = " + i + " degree = " + n.Degree)
      i += 1
    }
    
    val start_time = wall_time()
    var C = 0.0
    var num = 0
    while (num < 4) {
    
    	val Din = Sum(G.Nodes, (u:Rep[Node]) => member(u) == num){ _.Degree }
    	val Dout = Sum(G.Nodes, (u:Rep[Node]) => member(u) != num){ _.Degree }
    	val Cross = Sum(G.Nodes, (u:Rep[Node]) => member(u) == num){ u =>
                   Count(u.Nbrs) {j => member(j) != num}}
    	val m = if (Din < Dout) Din else Dout
    	val retVal = if(m == 0) {
    		if(Cross == 0) 0.0 else MAX_DOUBLE
    	}
    	else {
    		Cross.asInstanceOf[Rep[Double]] / m.asInstanceOf[Rep[Double]]
    	}
    	C += retVal
    	num += 1
    }
    println("TIME_LOOP: " + (wall_time() - start_time))
    
    
    //println("Din = " + Din)
    //println("Dout = " + Dout)
    //println("Retval = " + retVal)
  }
  
  def SSC() {
    val G = rand_graph()
    val CompID = NodeProperty[Int](G)
    
    val start_time = wall_time()
    var numC = 0
    val P = NodeOrder()
    CompID.setAll(-1)
    
    For/*[Node, GIterable[Node]]*/(G.Nodes, (t:Rep[Node]) => !(P.Has(t))) { t => 
      InDFS(G, t, (s:Rep[Node]) => !P.Has(s), { t => unit()}, 
      { InPost(s => P.PushFront(s)) })
    }
    
    For(P.Items, (s:Rep[Node]) => CompID(s) == -1) { s => 
      InBFS(G^, s, /*(t:Rep[Node]) => CompID(s) == -1,*/ { t => 
        if(CompID(s) == -1) CompID(t) = numC }) 
      numC += 1;
    }
    println("TIME_LOOP: " + (wall_time() - start_time))
    println("NumC = " + numC)
  }
  
  def BC() {
    val G = rand_graph()
    val BC = NodeProperty[Float](G, 0f)
    
    for(s <- G.Nodes) {
    	// define temporary properties
    	val Sigma = NodeProperty[Float](G); 
    	val Delta = NodeProperty[Float](G);
    	Sigma(s) = 1f;
   
    	// Traverse graph in BFS-order from s
    	InBFS(G, s, (v:Rep[Node]) => v!=s, { v =>
    		// sum over BFS-parents
    		Sigma(v) = Sum(v.UpNbrs) {w => Sigma(w)};
    	}, 
    	InReverse({v => 
      		// sum over BFS-children
    		Delta(v) = Sum(v.DownNbrs) { w =>
    			Sigma(v) / Sigma(w) * (1f + Delta(w))
    		}
    		// TODO: this needs to be a reduction op
    		BC(v) += Delta(v); //accumulate BC
    	}))
    }
  }
  
  def main() {
    /* tests */
    //test_graphOps()
    //test_nodeOpsEdgeOps()
    //test_nodeEdgeProps()
    //test_reduceable()
    //test_reductions()
    //test_deferrable()
    //test_filters()
    //test_iterations()
    //test_traversals()
    
    /* sample applications */
    //conductance()
//    var i = 0
//    while (i < 2) {
      page_rank()
//      i += 1
//    }
    //SSC()
    //basic()
    //BC()
  }
}

