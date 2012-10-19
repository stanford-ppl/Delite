package ppl.dsl.opticvx.problem

/*
trait Cone {
  val nIntParams: Int
  val shape: Shape
  if (shape.nIntParams != nIntParams) throw new ProblemIRValidationException()
}

case class ConeFor(val size: Size, val body: Cone) extends Cone {
  val nIntParams: Int = size.nIntParams
  val shape: Shape = ShapeFor(nIntParams, size, body.shape)
  
  if (size.nIntParams != nIntParams) throw new ProblemIRValidationException()
  if (body.nIntParams != (nIntParams + 1)) throw new ProblemIRValidationException()
}

case class ConeStruct(val body: Seq[Cone]) extends Cone {
  val nIntParams: Int = body(0).nIntParams
  val shape: Shape = ShapeStruct(nIntParams, body map ((x) => x.shape))
  
  for (b <- body) {
    if (b.nIntParams != nIntParams) throw new ProblemIRValidationException()
  }
}

//The trivial scalar cone (the only proper cone over R)
case class ConeScalar(val nIntParams: Int) extends Cone {
  val shape: Shape = ShapeScalar(nIntParams)
}

//Second order cone
case class ConeSecondOrder(val size: Size) extends Cone {
  val nIntParams: Int = size.nIntParams
  val shape: Shape = ShapeStruct(nIntParams, Seq[Shape](
    ShapeScalar(nIntParams), 
    ShapeFor(nIntParams, size, ShapeScalar(nIntParams+1))))
}
*/
