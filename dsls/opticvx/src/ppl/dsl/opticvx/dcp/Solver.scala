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
  
  // Computes m_dst = m_dst * s_scale_dst + m_src * s_scale_src
  case class SolverOpMMpy(
    val input: Shape,
    val memory: Seq[Shape],
    val almap: Almap,
    val m_src: Int,
    val m_dst: Int,
    val s_scale_src: Int,
    val s_scale_dst: Int
  )
  
  
}