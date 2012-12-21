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
    //Create an instance of a directed mutable graph
    val mg = DMutableGraph()
    //Add some nodes
    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    //Add some edges
    val me1 = mg.AddEdge(mn1, mn2)
    val me2 = mg.AddEdge(mn1, mn2) //this one is a repeated edge
    val me3 = mg.AddEdge(mn2, mn1)
    val me4 = mg.AddEdge(mn1, mn1) //this one is a self-edge
    //Create an immutable snapshot of the mutable graph
    val g = mg.Snapshot

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

    // TODO Find a new way to test "getNode" and "getEdge" since we're using Mutable Edges/Nodes
    /*
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
    */

    val nodes = g.Nodes.toSet
    val edges = g.Edges.toSet
    if(nodes.Size != 2 || edges.Size != 4) {
      println("[FAIL] Wrong nodes/edges collection size")
    } else {
      println("[OK] Correct nodes/edges collection size")
    }

    // TODO Find a new way to test GSet "Has"
    /*
    if((!nodes.Has(n1)) || (!nodes.Has(n2)) || (!edges.Has(e1)) || (!edges.Has(e2))
        || (!edges.Has(e3)) || (!edges.Has(e4))) {
      println("[FAIL] Wrong nodes/edges collection contents")
    } else {
      println("[OK] Correct nodes/edges collection contents")
    }
    */

    //---------//

    //Create an instance of a directed mutable graph
    val mg2 = DMutableGraph()
    //Add some nodes
    val mn3 = mg2.AddNode
    val mn4 = mg2.AddNode
    //Add some edges
    val me5 = mg2.AddEdge(mn3, mn4)
    val me6 = mg2.AddEdge(mn3, mn4)
    val me7 = mg2.AddEdge(mn4, mn3)
    val me8 = mg2.AddEdge(mn3, mn3)
    //Create an immutable snapshot of the mutable graph
    val g_s = mg2.Snapshot

    if(g_s.NumNodes != mg2.NumNodes || g_s.NumEdges != mg2.NumEdges) {
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

    //Create an instance of an undirected mutable graph
    val mg3 = UMutableGraph()
    //Add some nodes
    val mn5 = mg3.AddNode
    val mn6 = mg3.AddNode
    //Add an edge
    val me9 = mg3.AddEdge(mn5, mn6)
    //Create an immutable snapshot of the mutable graph
    val g_s2 = mg3.Snapshot

    // TODO Same as above, find a new way to test node relations after graph construction
    // since right now a MutableNode != Node
    /*
    if((n5.OutDegree != 1) && (n6.OutDegree != 1) && (n5.InDegree != 1) && (n6.InDegree != 1)) {
      println("[FAIL] Undirected graph wrong edge connectivity")
    } else {
      println("[OK] Undirected graph connectivity is correct")
    }
    */
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

//TODO foreach is not working right now
/*
  def test_reduceable() {
    val g  = Graph()
    val n1 = g.AddNode
    val n2 = g.AddNode
    val n3 = g.AddNode
    val n4 = g.AddNode
    g.Freeze

    println("Test Foreach With No Reductions")
    for (n <- g.Nodes) {
      println("should print 4 times")
    }

    println("Test Reduction Assignments: SUM ")
    val sum = Reduceable[Int](5)
    for(n <- g.Nodes) {
      println("foo")
      sum += 1
      println("bar")
      // println(sum)  // TODO: won't work --> need to go through remaining effects and remove any remaining references to the DeliteReduction sum += 1
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
      println("max: " + max.value) // should at least not crash
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
*/

  def test_reductions() {
    val mg = DMutableGraph()
    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    val mn3 = mg.AddNode
    val mn4 = mg.AddNode

    val g = mg.Snapshot

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

// TODO BFS and DFS traversals are not working right now
/*
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
*/

  // TODO these tests fail because the Snapshot is not
  // saving the outNeighbors, inNeighbors, outEdges, inEdges, ..., etc.
  // correctly
  def test_nodeOpsEdgeOps() {
    val mg = DMutableGraph()

    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    val mn3 = mg.AddNode
    val mn4 = mg.AddNode
    val me1 = mg.AddEdge(mn1, mn2)
    val me2 = mg.AddEdge(mn1, mn3)
    val me3 = mg.AddEdge(mn2, mn1)
    val me4 = mg.AddEdge(mn4, mn1)
    val me5 = mg.AddEdge(mn3, mn4)
    val g = mg.Snapshot

    // Grab the immutable nodes and edges from the snapshot
    val n1 = g.Node(0)
    val n2 = g.Node(1)
    val n3 = g.Node(2)
    val n4 = g.Node(3)
    val e1 = g.Edge(0)
    val e2 = g.Edge(1)
    val e3 = g.Edge(2)
    val e4 = g.Edge(3)
    val e5 = g.Edge(4)

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
    //TODO these filters introduce delitec "illegal sharing of mutable objects" errors
    //as a result of the DeliteOpFilter. How to fix?
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
    //TODO foreach is not working right now
/*
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
*/
  }

//TODO iterations aren't working right now
/*
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

    // TODO: this test fails with fusion on. weird interaction inside DeliteOpForeachReduce?
    // inserting printlns effects things in non-obvious ways, too... maybe has to do with
    // the order things get fused in? the two foreaches should *not* get fused due to the
    // write of np and the subsequent read.. HOWEVER, they are getting fused! are we losing the write?
    val i = Reduceable[Int](0)
    for(n <- G.Nodes) {
      // println("mark")
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
      println("np(n1): " + np(n1))
      println("np(n2): " + np(n2))
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
*/

  def test_nodeEdgeProps() {
    val mg = DMutableGraph()

    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    val me1 = mg.AddEdge(mn1, mn2)
    val me2 = mg.AddEdge(mn2, mn1)
    val me3 = mg.AddEdge(mn1, mn1)
    val g  = mg.Snapshot

    // Grab the immutable nodes and edges
    val n1 = g.Node(0)
    val n2 = g.Node(1)
    val e1 = g.Edge(0)
    val e2 = g.Edge(1)
    val e3 = g.Edge(2)

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

  // Convenience method for generating a Graph.
  // Not actually random.
  def rand_graph(): Rep[Graph] = {
    val mg = DMutableGraph()
    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    val mn3 = mg.AddNode
    val mn4 = mg.AddNode
    val mn5 = mg.AddNode

    mg.AddEdge(mn1, mn2)
    mg.AddEdge(mn1, mn3)
    mg.AddEdge(mn2, mn4)
    mg.AddEdge(mn3, mn5)
    mg.Snapshot
  }

  def basic() {
    // Construct a graph
    val mg = MutableGraph()
    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    val mn3 = mg.AddNode
    val mn4 = mg.AddNode
    val me = mg.AddEdge(mn1, mn2)
    // Create an immutable snapshot of the graph
    val g = mg.Snapshot

    println("Graph construction finished")
    println("Num nodes: " + g.NumNodes)
    println("Num edges: " + g.NumEdges)
    println("Nodes: " + g.Nodes)
    println("Node 1: " + mn1)
    println("Node 2: " + mn2)
    println("Node 3: " + mn3)
    println("Node 4: " + mn4)
    println("Edges: " + g.Edges)
    //TODO connect mutable nodes and nodes?
    val n1 = g.Node(0)
    val n2 = g.Node(1)
    val n3 = g.Node(2)
    val n4 = g.Node(3)
    println("Node 1 num neighbors: " + n1.NumNbrs)
    println("Node 2 num neighbors: " + n2.NumNbrs)

    println("FOR:")
    For[Node, GIterable[Node]](g.Nodes) { n => println("Node: " + n) }

    println("NODE PROP")
    val np = NodeProperty[Int](g, 1)
    println("Created")
    println("n1.np = " + np(n1))
    println("n2.np = " + np(n2))
    np(n1) = 5
    np(n2) = 6
    println("n1.np = " + np(n1))

    val np2 = NodeProperty[String](g, "a")
    val np3 = NodeProperty[Boolean](g)
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

    // TODO traversal
    //println("IN BFS")
    //InBFS(g, n1, n=>println("node prop: " + np(n)))

    println("NODE ORDER")
    val no = NodeOrder()
    no.PushBack(n1)
    no.PushBack(n2)
    no.PushBack(n3)
    no.PushBack(n4)

    println("Node order size = " + no.Size)
    println("Node order contains n1 = " + no.Has(n1))

    // TODO foreach
    /*
    for(i <- no.Items) {
       println("Node: " + i)
    }
    */
  }

  def main() {
    /* tests */
    test_graphOps()
    test_deferrable()
    test_reductions()
    test_filters()
    test_nodeOpsEdgeOps()
    test_nodeEdgeProps()
    basic()

    //TODO these aren't working right now
    /*
    test_reduceable()
    test_iterations()
    test_traversals()
    */

  }
}

