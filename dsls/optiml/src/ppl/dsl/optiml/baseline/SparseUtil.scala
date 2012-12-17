package ppl.dsl.optiml.baseline

trait SparseUtil {
  
  def bsearch(a: Array[Int], _start: Int, _end: Int, pos: Int): Int = {
    // binary search index for pos
    var start = _start
    var end = _end
    var mid = (start+end)/2
    var found = false    
    while (!found && (start <= end)) {
      mid = (start+end)/2
      if (pos > a(mid)) {        
        start = mid + 1
      }
      else if (pos < a(mid)) {
        end = mid - 1
      }
      else {
        found = true
      }
    }

    if (found) mid 
    else {
      // maps to a reversible negative number representing the index to insert at if not found
      if (_end < _start) ~(_start) 
      else if (pos > a(mid)) ~(mid+1) 
      else ~mid 
    }
  }   
}