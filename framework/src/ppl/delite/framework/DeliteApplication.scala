package ppl.delite.framework

import scala.virtualization.lms.ppl.ScalaOpsPkgExp

trait DeliteApplication extends ScalaOpsPkgExp {

  var args: Rep[Array[String]] = _

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = args;
    println("Running the main function to extract the AST")
    main()

  }

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit


  //so that our main doesn't itself get lifted
  private def println(s:String) = System.out.println(s)
}
