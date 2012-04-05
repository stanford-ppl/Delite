package ppl.dsl.experimental

/**
 * This trait encodes a target for code generation. Each DSL package must provide a code generator package for a
 * particular target via the method getCodeGenPkg in DeliteApplication.
 *
 * In the future, target may be expanded to include machine models, parameters, etc.
 */
trait SandboxTarget {
  val IR: SandboxDeliteApplication
  import IR._

  val name: String  
}