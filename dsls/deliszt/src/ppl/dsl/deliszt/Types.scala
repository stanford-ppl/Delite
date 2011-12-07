package ppl.dsl.deliszt

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * DeLiszt compiler types
 */

//////////////////
// DeLizst

trait Vec[N<:IntM,T] extends DeliteCollection[T]
trait Mat[R<:IntM,C<:IntM,T] extends DeliteCollection[T]

trait VecView[N<:IntM,T] extends Vec[N,T]
trait MatRow[C<:IntM,T] extends VecView[C,T]
trait MatCol[R<:IntM,T] extends VecView[R,T]

// Mesh objects
trait MeshObj
trait Mesh extends MeshObj
trait Cell extends MeshObj
trait Edge extends MeshObj
trait Face extends MeshObj
trait Vertex extends MeshObj

// Mesh set
trait MeshSet[MO <: MeshObj] extends DeliteCollection[MO]
trait BoundarySet[MO<:MeshObj] extends MeshSet[MO]

// Fields
trait CRS
trait Field[+MO<:MeshObj,T] extends DeliteCollection[T]
trait LabelField[MO<:MeshObj,T] extends DeliteCollection[T]
