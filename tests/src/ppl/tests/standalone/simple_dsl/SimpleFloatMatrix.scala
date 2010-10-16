//package ppl.tests.standalone.simple_dsl
//
//import ppl.delite.framework.{DeliteApplication, DSLType}
//import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
//import java.io.PrintWriter
//import ppl.delite.framework.codegen.c.CodeGeneratorCBase
//
//trait SimpleFloatMatrix extends DSLType { this: DeliteApplication =>
//
//  case class MatrixZeros(n: Rep[Int]) extends Def[SimpleFloatMatrix]
//  case class MatrixPlus(v1: Rep[SimpleFloatMatrix], v2: Rep[SimpleFloatMatrix]) extends Def[SimpleFloatMatrix]
//  case class MatrixPPrint(v: Rep[SimpleFloatMatrix]) extends Def[String]
//
//  def mzeros(n: Rep[Int]): Rep[SimpleFloatMatrix] = reflectEffect(MatrixZeros(n))
//  def infix_+(m1: Rep[SimpleFloatMatrix], m2: Rep[SimpleFloatMatrix])(implicit ef: MatrixErasureFix): Rep[SimpleFloatMatrix] = MatrixPlus(m1, m2)
//
//  class SimpleFloatMatrixOps(m: Rep[SimpleFloatMatrix]) {
//    def pprint: Rep[String] = MatrixPPrint(m)
//  }
//
//  //todo, need to be able to only import this stuff automatically
//  implicit def injectOpsSFM(v:Rep[SimpleFloatMatrix]) = new SimpleFloatMatrixOps(v)
//
//  //todo, cleanup these erasure fixes
//  class MatrixErasureFix
//  implicit val mef = new MatrixErasureFix
//
//  //code gen
//  targetCodeGenerators.get("Scala").getOrElse(
//    throw new RuntimeException("Couldn't find Scala code generator")
//  ) += new SimpleFloatMatrixGeneratorScala {
//    val intermediate: SimpleFloatMatrix.this.type = SimpleFloatMatrix.this
//  }
//  targetCodeGenerators.get("C").getOrElse(
//    throw new RuntimeException("Couldn't find C code generator")
//  ) += new SimpleFloatMatrixGeneratorC {
//    val intermediate: SimpleFloatMatrix.this.type = SimpleFloatMatrix.this
//  }
//}
//
//
////code generation
//trait SimpleFloatMatrixGeneratorScala extends CodeGeneratorScalaBase {
//
//  val intermediate: SimpleFloatMatrix with DeliteApplication
//  import intermediate._
//
//  //code generation bit
//  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
//    case MatrixZeros(n) => emitValDef("",sym, "Matrix.zeros(" + quote(n) + ")")
//    case MatrixPlus(m1,m2) => emitValDef("",sym, quote(m1) + " + " + quote (m2))
//    case MatrixPPrint(m) => emitValDef("",sym, quote(m) + ".pprint")
//    case _ => super.emitNode(sym, rhs)
//  }
//}
//
//trait SimpleFloatMatrixGeneratorC extends CodeGeneratorCBase {
//
//  val intermediate: SimpleFloatMatrix with DeliteApplication
//  import intermediate._
//
//  //code generation bit
//  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
//    //todo replace the manifest with embedded types
//    case MatrixZeros(n) => emitValDef("matrix", sym, "Matrix.zeros(" + quote(n) + ")")
//    case MatrixPlus(m1,m2) => emitValDef("matrix", sym, quote(m1) + " + " + quote (m2))
//    case MatrixPPrint(m) => emitValDef("matrix", sym, quote(m) + ".pprint()")
//    case _ => super.emitNode(sym, rhs)
//  }
//}