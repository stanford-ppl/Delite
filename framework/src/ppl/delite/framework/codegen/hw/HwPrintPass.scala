package ppl.delite.framework.codegen.hw
class HwPrintPass extends HwPass {
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
