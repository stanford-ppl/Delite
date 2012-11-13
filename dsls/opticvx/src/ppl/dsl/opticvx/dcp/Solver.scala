package ppl.dsl.opticvx.dcp

trait DCPSolver {
  self: DCPShape with DCPCone with DCPAlmap =>

  case class Solver(
    val input: Shape,
    val memory: Seq[Shape],
    val scalars: Int,
    val code: Seq[SolverOp]
  )
  
  trait SolverOp

  sealed trait Scale
  case class ScaleZero extends Scale
  case class ScaleOne extends Scale
  case class ScaleNeg extends Scale
  case class ScaleConst(val c: Double) extends Scale
  case class ScaleVar(val s: Int) extends Scale
  
  // Computes m_dst = m_dst * s_scale_dst + almap * m_src * s_scale_src
  case class SolverOpMMpy(
    val input: Shape,
    val memory: Seq[Shape],
    val almap: Almap,
    val m_src: Int,
    val m_dst: Int,
    val s_scale_src: Scale,
    val s_scale_dst: Scale
  ) extends SolverOp
  
  // Computes m_dst = m_dst * s_scale_dst + almap * 1 * s_scale_src
  case class SolverOpPut(
    val input: Shape,
    val memory: Seq[Shape],
    val almap: Almap,
    val m_dst: Int,
    val s_scale_src: Scale,
    val s_scale_dst: Scale
  ) extends SolverOp

  // Computes the sum of two variables
  case class SolverOpAdd(
    val input: Shape,
    val memory: Seq[Shape],
    val m_src_1: Int,
    val m_src_2: Int,
    val s_scale_src_1: Scale,
    val s_scale_src_2: Scale,
    val s_scale_dst: Scale
  ) extends SolverOp

  // Computes the norm squared of a variable
  case class SolverOpNorm(
    val input: Shape,
    val memory: Seq[Shape],
    val m_src: Int,
    val s_dst: Int
  ) extends SolverOp

  // Computes the sum of two scalars
  case class SolverOpAddScalar(
    val input: Shape,
    val memory: Seq[Shape],
    val s_src_1: Int,
    val s_src_2: Int,
    val s_dst: Int
  ) extends SolverOp

  // Computes the sum of two scalars
  case class SolverOpMulScalar(
    val input: Shape,
    val memory: Seq[Shape],
    val s_src_1: Int,
    val s_src_2: Int,
    val s_dst: Int
  ) extends SolverOp

  // Computes the square root of a scalar
  case class SolverOpSqrtScalar(
    val input: Shape,
    val memory: Seq[Shape],
    val s_src: Int,
    val s_dst: Int
  ) extends SolverOp

  // Computes the multiplicative inverse of a scalar
  case class SolverOpSqrtScalar(
    val input: Shape,
    val memory: Seq[Shape],
    val s_src: Int,
    val s_dst: Int
  ) extends SolverOp

  // Sets a scalar
  case class SolverOpSetScalar(
    val input: Shape,
    val memory: Seq[Shape],
    val c_src: Double,
    val s_dst: Int
  ) extends SolverOp

  // Sets a vector to zero
  case class SolverOpSetZero(
    val input: Shape,
    val memory: Seq[Shape],
    val m_dst: Int
  ) extends SolverOp

}