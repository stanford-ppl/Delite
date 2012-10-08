package ppl.dsl.opticvx.dcp

trait DCPShape {

  type Size

  sealed abstract class Shape
  
  case class ShapeScalar extends Shape
  case class ShapeFor(val size: Size, val body: Shape) extends Shape
  case class ShapeStruct(val body: Seq[Shape]) extends Shape

  
  sealed abstract class VexityDesc {
    def shape: Shape
  }
  
  case class VexityDescScalar(val vexity: Signum, val sign: Signum) extends VexityDesc {
    def shape: Shape = ShapeScalar()
  }
  case class VexityDescFor(val size: Size, val body: VexityDesc) extends VexityDesc {
    def shape: Shape = ShapeFor(size, body.shape)
  }
  case class VexityDescStruct(val body: Seq[VexityDesc]) extends VexityDesc {
    def shape: Shape = ShapeStruct(body map ((s) => s.shape))
  }
  
  
  sealed abstract class TonicityDesc {
    def shape: Shape
  }
  
  case class TonicityDescScalar(val tonicity: Signum, val niltonicity: Signum) extends TonicityDesc {
    def shape: Shape = ShapeScalar()
  }
  case class TonicityDescFor(val size: Size, val body: TonicityDesc) extends TonicityDesc {
    def shape: Shape = ShapeFor(size, body.shape)
  }
  case class TonicityDescStruct(val body: Seq[TonicityDesc]) extends TonicityDesc {
    def shape: Shape = ShapeStruct(body map ((s) => s.shape))
  }
  
  /* A shape given some number of integer sizes is a:
   * - scalar
   * - a homogenous composition
   * - a heterogenous composition
   */
  
}