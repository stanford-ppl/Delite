package ppl.delite.framework.codegen.hw

trait HwPass {

  val IR: ThorIR = null
  import IR._

  def visit(m: CAdd)
  def visit(m: CSub)
  def visit(m: CMul)
  def visit(m: CDiv)
  def visit(m: CMux)
  def visit(m: Dummy)
  def visit(m: CComposite)
  def visit(m: Pipeline)
  def visit(m: Parallel)
  def visit(m: TreeRed)
  def visit(m: AGU)
  def visit(m: RU)

  def matchAndVisit(m: Module) = m match {
    case s: CAdd => visit(s)
    case s: CSub => visit(s)
    case s: CMul => visit(s)
    case s: CDiv => visit(s)
    case s: CMux => visit(s)
    case s: Dummy => visit(s)
    case s: CComposite => visit(s)
    case s: Pipeline => visit(s)
    case s: Parallel => visit(s)
    case s: TreeRed => visit(s)
    case s: AGU => visit(s)
    case s: RU => visit(s)
    case _ => throw new Exception("No visitor defined for this node")
  }


  def doItDfS(m: Module) = {
    throw new Exception("doItDfs not implemented yet!")
  }

  def doItBfs(m: Module) = {
    throw new Exception("doItBfs not implemented yet!")
  }

  def doIt(m: Module) = {
    matchAndVisit(m)
  }

  def doItLinear(modules: List[Module]) = {
    throw new Exception("doItLinear not implemented yet!")
  }
}

class PrintPass extends HwPass {
  import IR._

  override def visit(m: CAdd) = {
    println("CAdd")
  }

  override def visit(m: CSub) = {
    println("CSub")
  }

  override def visit(m: CMul) = {
    println("CMul")
  }

  override def visit(m: CDiv) = {
    println("CDiv")
  }

  override def visit(m: CMux) = {
    println("CMux")
  }

  override def visit(m: Dummy) = {
    println("Dummy")
  }
  override def visit(m: CComposite) = {
    println("CComposite")
  }

  override def visit(m: Pipeline) = {
    println("Pipeline")
  }

  override def visit(m: Parallel) = {
    println("Parallel")
  }

  override def visit(m: TreeRed) = {
    println("TreeRed")
  }

  override def visit(m: AGU) = {
    println("AGU")
  }

  override def visit(m: RU) = {
    println("RU")
  }
}
