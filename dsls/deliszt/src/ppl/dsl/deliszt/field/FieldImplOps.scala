package ppl.dsl.deliszt.field

import scala.virtualization.lms.common._

import ppl.dsl.deliszt._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/26/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait FieldImplOps { this: DeLiszt =>
//  def field_obj_const_impl[MO<:MeshObj:Manifest,VT:Manifest](v : Rep[VT]): Rep[Field[MO,VT]]
}

trait FieldImplOpsStandard extends FieldImplOps {
  this: DeLisztCompiler with DeLisztLift =>

  //////////////////////////
  // kernel implementations
  /* def field_obj_const_impl[MO<:MeshObj:Manifest,VT:Manifest](c : Rep[VT]) = {
    val v = Field[MO,VT]()
    var i = 0
    while(i < v.size) {
      v(i) = c
      i += 1
    }
    v.unsafeImmutable
  } */
}
