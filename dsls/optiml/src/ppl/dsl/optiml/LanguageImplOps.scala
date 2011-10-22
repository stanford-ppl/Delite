package ppl.dsl.optiml

trait LanguageImplOps { this: OptiML =>
  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A]
  def optiml_untilconverged_impl[V <:Vertex :Manifest, E <: Edge :Manifest](
     g: Rep[Graph[V, E]], block: Rep[V] => Rep[Unit]): Rep[Unit]
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: OptiMLCompiler with OptiMLLift =>
  
  def optiml_untilconverged_impl[V <: Vertex : Manifest, E <: Edge : Manifest](g: Rep[Graph[V, E]], block: Rep[V] => Rep[Unit]) = {
    val vertices = g.vertices

    val tasks = vertices.cloneL
    //val tasks = repCloneableToCloneableOps(vertices)(vectorCloneable[V],manifest[Vertices[V]]).cloneL
    val seen = Set[V]()
    
    while(tasks.length > 0) {
      tasks.foreach(block)
      tasks.clear()
      var totalTasks = unit(0)
      
      for(i <- 0 until vertices.length) {
        val vtasks = vertices(i).tasks
        totalTasks += vtasks.length
        for(j <- 0 until vtasks.length) {
          val task = vtasks(j).asInstanceOfL[V]
          if(!seen.contains(task)) {
            tasks += task
            seen.add(task)
          }
        }

        vertices(i).clearTasks()
      }

      seen.clear()
    }
  }

  def optiml_untilconverged_impl[A:Manifest:Cloneable](
     x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
     block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A] = {

    var delta = unit(scala.Double.MaxValue)
    var prev = unit(null).asInstanceOfL[A]
    var next = x
    var iter = unit(0)

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.cloneL()
      else
        prev = next

//      try{
        next = block(next)
//      }
//      catch{
//        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
//      }
      iter += 1
      delta = diff(next, prev)
      //println("(" + delta + ")")
    }

      if (iter == max_iter){
        //throw new ConvergenceException("Maximum iterations exceeded")
        println("Maximum iterations exceeded")
        returnL()
      }

    next
  }
}
