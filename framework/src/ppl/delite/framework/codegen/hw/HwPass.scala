package ppl.delite.framework.codegen.hw

trait HwPass {
  val IR: ThorIR = null
  import IR._

  def doIt(m: Module)
}

class PrintPass extends HwPass {
  import IR._

  override def doIt(m: Module) = m match {
    case s: CAdd =>
      println("CAdd")
    case s: CSub =>
      println("CSub")
    case s: CMul =>
      println("CMul")
    case s: CDiv =>
      println("CDiv")
    case s: CMux =>
      println("CMux")
    case s: CComposite =>
      println("CComposite")
    case s: Pipeline =>
      println("Pipeline")
    case s: Parallel =>
      println("Parallel")
    case s: TreeRed =>
      println("TreeRed")
    case s: FSM =>
      println("FSM")
    case s: AGU =>
      println("AGU")
    case s: RU =>
      println("RU")
    case _ => throw new Exception("Don't know how to handle this node")
  }
}
