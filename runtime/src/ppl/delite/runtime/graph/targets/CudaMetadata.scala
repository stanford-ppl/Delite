package ppl.delite.runtime.graph.targets

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 10:48:52 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

final class CudaMetadata {

  var blockSizeX: String = _
  var blockSizeY: String = _
  var blockSizeZ: String = _
  var dimSizeX: String = _
  var dimSizeY: String = _
  var inputs: List[String] = Nil
  var inputTypes: List[String] = Nil
  var temps: List[String] = Nil
  var outputAlloc: String = _
  var outputSet: String = _

}
