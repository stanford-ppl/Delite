package ppl.apps.bio.spade

import ppl.dsl.optiml._

/**
 * This trait was implemented by a student a long time ago but doesn't use the DSL
 * properly. Specifically, it uses the following structures that don't properly exist:
 * 
 * ACluster: supposed to be a class representing a cluster with a bunch of helper methods.
 * APair: supposed to be a tuple of (ACluster, Double)
 * PQ: supposed to be a priority queue of pairs. 
 * 
 * Pure scala implementation copied at the bottom of the file for reference - but all of
 * these need to be implemented in a lifted way.
 */
 
trait Clustering {
  this: OptiMLApplication =>
  /*
  def cluster(d: Rep[DenseMatrix[Double]], k: Rep[Int]): Rep[DenseVector[Int]] = {
    val obs     = d.numRows
    val dim     = d.numCols
    val data    = DenseVector[Double](obs*dim, true)
    val centers = DenseVector[Double](obs*dim, true)
    println("obs = " + obs + ", dim = " + dim + ", k = " + k)

    var idx = 0
    var i = 0
    while(i < obs){
      var j = 0
      while(j < dim){
        data(idx) = d(i)(j)
        centers(idx) = data(idx);
        idx += 1
        j += 1
      }
      i += 1
    }

    val c_ap = DenseVector[ACluster](obs, true)
    i = 0
    while(i < obs){
      c_ap(i) = ACluster(dim)
      val members = DenseVector[Int](1, true)
      c_ap(i).init_RM(data, centers, members, i)
      i += 1
    }

    val ac_valid = c_ap
    var flag = true
    var round = 0
    while(flag){
      //ac_valid = ac_valid.partition(_.valid)._1
      ac_valid mfilter {a:Rep[ACluster]=> a.valid}

      println("round = " + round + ", num_valid = " + ac_valid.length)

      if(round==5){
        // TODO: following is the concise way, but cannot pass the mutable check
        //ac_valid = ac_valid map {e=> if(e.num_members==1) e.valid_=(false); e}
        for(i <- (0::ac_valid.length-1)){
          if(ac_valid(i).num_members==1)
            ac_valid(i).valid_=(false)
        }
        //ac_valid = ac_valid.partition(_.valid)._1
        ac_valid mfilter {_.valid}
      }

      val num_valid = ac_valid.length
      // TODO: anyway to write the line below more elegant?
      if(num_valid.AsInstanceOf[Double] < 1.5*k.AsInstanceOf[Double]){
        flag = false
      }
      else{
        if(round!=0){
          // TODO: sort is non-mutable
          //ac_valid = ac_valid.sort
          ac_valid.msort
          // TODO: following is the concise way, but cannot pass the mutable check
          //ac_valid.foreach{ _.reset_RM }
          for(i <- (0::ac_valid.length-1))
            ac_valid(i).reset_RM
        }
        val limit = max(1, num_valid/5000).AsInstanceOf[Int]
/*
        // TODO: Concise
        // for(ac_into <- ac_valid if !ac_into.merged){
        for(ac_into <- ac_valid){
          if(!ac_into.merged){
            ac_into.merged_=(true)
            val candidates = (0::num_valid-1) map {from=> ac_into.getCandidates(ac_valid(from))}
            ac_into.mergeCandidates(candidates, limit)
          }
        }
*/

        // TODO: Imperative
        var into = 0
        while (into < num_valid){
          if (!ac_valid(into).merged){
            ac_valid(into).merged_=(true)
            val num_threads = 8
            val pq = PQ(limit)
            val pq_p = DenseVector[PQ](num_threads, true)
            for (tid <- (0::num_threads)){
              var begin = tid / num_threads * num_valid
              var end   = (tid+1) / num_threads * num_valid
              end = if (end > num_valid) num_valid else end
              pq_p(tid) = PQ(limit)
              while (begin < end){
                if (begin != into)
                  ac_valid(into).push_on_pq(ac_valid(begin), pq_p(tid))
                begin += 1
              }
            }
            var i = 0
            while (i < num_threads){
              pq.merge(pq_p(i))
              i += 1
            }
            ac_valid(into).merge_in_pq(pq)
          }
          into += 1
        }

/*
        // TODO: Sequential
        var into = 0
        while(into < num_valid){
          if(!ac_valid(into).merged){
            ac_valid(into).merged_=(true)
            val pq = PQ(limit)
            val offset_center = ac_valid(into).offset
            val offset_end = offset_center + dim
            var from = 0
            while(from < num_valid){
              if(into != from){
                // note: ac_valid(into).push_on_pq(ac_valid(from), pq)
                val from_cluster = ac_valid(from)
                var i = 0
                val num_members = from_cluster.num_members
                while(i < num_members){
                  // note: val d = absdist(into_center, from_cluster.members(i))
                  var offset0 = offset_center
                  var offset1 = from_cluster.members(i)
                  // note: following expression doesn't work
                  //       var d: Double = 0
                  //       d += abs(tmp)
                  var d = 0.0
                  while(offset0 < offset_end){
                    val tmp = centers(offset0) - data(offset1)
                    d += abs(tmp)
                    offset0 += 1
                    offset1 += 1
                  }
                  pq.push(from_cluster, d)
                  i += 1
                }
              }
              from += 1
            }
            ac_valid(into).merge_in_pq(pq)
          }
          into += 1
        }
        // System.exit(-1)
*/
        round += 1
      }

    }

    // Assignment and clean up
    val assgn = DenseVector[Int](obs, true)
    //val ac_valid = ac_valid.partition(_.valid)._1
    ac_valid mfilter {a:Rep[ACluster]=> a.valid}
    var cur_Id = 0
    while(cur_Id < ac_valid.length){
      var b = 0
      val num_members = ac_valid(cur_Id).num_members
      val members = ac_valid(cur_Id).members
      while(b < num_members){
        assgn(members(b)/dim) = cur_Id+1
        b += 1
      }
      cur_Id += 1
    }

    assgn
  }
  */
}


/*
case class APair (_1: ACluster, _2: Double) extends Ordered[APair] {
  def compare(that: APair) = if(this._2 < that._2) -1 else if(this._2 > that._2) 1 else 0
}

class PQ(val fold:Int) {

  var Q = new java.util.PriorityQueue[APair]()
  var MMD: Double = scala.Double.MaxValue

  def empty = Q.isEmpty

  def normalize(){
    var flag = true
    while(!Q.isEmpty && flag){
      if(Q.element._2 >= MMD)
        Q.remove()
      else
        flag = false
    }
  }

  def push(c:ACluster, d:Double){
    if(c.merged)
      MMD = if (MMD>d) d else MMD
    else if (d < MMD){
      Q.add(APair(c,d))
      if (Q.size > fold)
        Q.remove()
    }
  }

  def push(p:APair){
    val c = p._1
    val d = p._2
    if(c.merged)
      MMD = if (MMD>d) d else MMD
    else if (d < MMD){
      Q.add(APair(c,d))
      if (Q.size > fold)
        Q.remove()
    }
  }

  def merge(pq: PQ){
    if(MMD > pq.MMD) MMD = pq.MMD
    while(!pq.empty) {
      push(pq.Q.element)
      pq.pop
    }
  }
  
  def top = Q.element._1

  def pop(){
    Q.remove()
  }
}

class ACluster(val dim: Int) extends Ordered[ACluster] {

  var centers: Vector[Double] = null
  var index: Int = -1
  var offset: Int = -1
  var valid: Boolean = false
  var merged: Boolean = false
  var members: Vector[Int] = null
  var num_members: Int = 0
  var data: Vector[Double] = null

  def compare(that:ACluster) = this.num_members - that.num_members

  def absdist(_data1:Vector[Double], offset1:Int, _data2:Vector[Double], offset2:Int, length:Int) : Double = {
    var sum:Double = 0
    var idx = 0
    while(idx < length){
      val tmp = _data1(offset1+idx) - _data2(offset2+idx)
      sum += java.lang.Math.abs(tmp)
      idx += 1
    }
    sum
  }

  def init_RM(d:Vector[Double], c:Vector[Double], m:Vector[Int], i:Int){
    data = d
    centers = c
    index = i
    offset = i*dim
    valid = true
    merged = false
    members = m
    members(0) = i*dim
    num_members = 1
  }
  
  def reset_RM(){
    merged = false
    var i = 0
    while(i < dim){
      ColLT_RM.set(i)
      nth_element(members, 0, num_members/2, num_members)
      centers(offset+i) = data(members(num_members/2)+i)
      i += 1
    }
  }
  
  def push_on_pq(from: ACluster, pq: PQ){
    var i = 0
    while(i < from.num_members){
      pq.push(from, absdist(centers, offset, from.data, from.members(i), dim))
      i += 1
    }
  }
  
  def getCandidates(from: ACluster) = {
    var d = scala.Double.MaxValue
    if(index != from.index){
      var i = 0
      while(i < from.num_members){
        val dist = absdist(centers, offset, from.data, from.members(i), dim)
        if (d > dist) d = dist
        i += 1
      }
    }
    APair(from, d)
  }

  def mergeCandidates(candidates:Vector[APair], fold:Int) {
    val pq = new PQ(fold)
    // for(candidate <- candidates) pq.push(candidate)
    val len = candidates.length
    var i = 0
    while(i < len){
      pq.push(candidates(i))
      i += 1
    }
    pq.normalize
    while(!pq.empty){
      val rhs = pq.top
      if(!rhs.merged) merge_in(rhs)
      pq.pop
    }
  }

  def merge_in_pq(pq: PQ) {
    pq.normalize
    while(!pq.empty){
      val rhs = pq.top
      if(!rhs.merged)
        this.merge_in(rhs)
      pq.pop
    }
  }

  def merge_in(rhs: ACluster){
    rhs.merged = true
    rhs.valid = false
    // members <<= rhs.members
    members.insertAll(num_members, rhs.members)
    num_members += rhs.num_members
    rhs.num_members = 0
  }

  object ColLT_RM {
    var col:Int = -1
    def set(c:Int) {col = c}
    def apply(l:Int, r:Int) = data(l+col) < data(r+col)
  }

  def insertion_sort (array:Vector[Int], first:Int, last:Int) {
    var current = first + 1
    while (current < last) {
      val tmp = array(current)
      var i = current
      var tmp1 = array(i-1)
      while(ColLT_RM(tmp, tmp1) && first<i-1){
        array(i) = tmp1
        i -= 1
        tmp1 = array(i - 1)
      }
      if(ColLT_RM(tmp,tmp1)){
        array(i-1) = tmp
        array(i)   = tmp1
      }
      else{
        array(i) = tmp
      }
      current += 1
    }
  }

  def quickPartition(array:Vector[Int], first:Int, last:Int):Int = {
    var ff = first
    var ll = last
    var f  = array(ff)
    var l  = array(ll - 1)
    var pivot = array(ff + (ll-ff)/2)

    if (ColLT_RM(pivot,f)) {
      if (ColLT_RM(f,l))  pivot = f
      else if (ColLT_RM(pivot,l))  pivot = l
    }
    else if (ColLT_RM(l,f))  pivot = f
    else if (ColLT_RM(l,pivot))  pivot = l

    ff -= 1
    while (true) {
      ff += 1
	    while (ColLT_RM(array(ff), pivot)){ ff+=1 }
      ll -= 1
      while (ColLT_RM(pivot, array(ll))){ ll-=1 }
      if (ff >= ll)  return ff
      val tmp = array(ff)
      array(ff) = array(ll)
      array(ll) = tmp
    }
    ff
  }

  def nth_element(array:Vector[Int], f:Int, nth:Int, l:Int){
    var first = f
    var last  = l
    while (last - first > 3) {
	    val cut = quickPartition(array, first, last);
	    if (cut <= nth)  first = cut;
	    else  last = cut;
      }
    insertion_sort(array, first, last);
  }

}
*/