package ppl.delite.framework.codegen.hw

import scala.virtualization.lms.internal._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._

import java.io.File
import java.io.PrintWriter

/*
 * Delite Hardware code generator - This file contains all the traits that implement
 * the backend for hardware generation. All the traits that implement code generators
 * for specific IR nodes (with overridden emitNode methods) along with the base
 * code generator are defined here. Changes to the hardware codegen must be done here
 *
 *               +--------------------+
 *               |   Generic Codegen  |
 *               +---------+----------+
 *                         |
 *                         |
 *                   +-----v------+        +------------------------+
 *                   |Hw Codegen  |        |GenericGenDeliteOps     |
 *                   +-+---------++        +-+----------------------+
 *                     |         |           |
 *                     |         |           |
 * +-------------------v--+     +v-----------v----+
 * |HwGenDeliteInternalOps|     | HwGenDeliteOps  |
 * +----------------------+     +-----------------+
 *
 */

trait HwCodegen extends GenericCodegen
{
  val IR: Expressions
  import IR._

  var kernelInputVals: List[Sym[Any]] = Nil
  var kernelInputVars: List[Sym[Any]] = Nil
  var kernelOutputs: List[Sym[Any]] = Nil

  override def deviceTarget: Targets.Value = Targets.Hw
  override def toString = "hw"

  override def kernelFileExt = "thor"

  def kernelName = "module_" + kernelOutputs.map(quote).mkString("")

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {

    def kernelSignature: String = {
      val out = new StringBuilder
      out.append(kernelName + "(")
      out.append(")")
      out.toString
    }
    stream.println(kernelSignature + " {")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {
      stream.println("}")
  }


  // Every type is remapped to a 'Module' for the hardware backend now
  override def remap[A](m: Manifest[A]) : String = {
    "Module"
  }

  override def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    super.isPrimitiveType(m)
  }

  // TERRIBLE name
  // This function returns the String representation of an Exp
  // in this backend
  override def quote(x: Exp[Any]) = {
    super.quote(x)
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  override def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println(remap(sym.tp) + " " + quote(sym) + ";")
  }

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(quote(sym) + " = " + rhs + ";")
  }

  override def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {
    kernelInputVals = vals
    kernelInputVars = vars
    kernelOutputs = syms
  }

  override def initializeGenerator(buildDir:String, args: Array[String]): Unit = {
    val outDir = new File(buildDir)
    outDir.mkdirs

    super.initializeGenerator(buildDir, args)
  }

  def emitSource[A:Manifest](args: List[Sym[_]], body: Block[A], functionName: String, out: PrintWriter) = {

    val sA = remap(manifest[A])

    withStream(out) {
      stream.println("// Hw Preamble")
      stream.println(sA+" "+functionName+"("+args.map(a => remap(a.tp)+" "+quote(a)).mkString(", ")+") {")
      emitBlock(body)
      val y = getBlockResult(body)

      stream.println("// Hw end source")
    }
    Nil
  }
}

trait HwGenDeliteOps extends HwCodegen with GenericGenDeliteOps
{
}

trait HwGenDeliteInternalOps extends HwCodegen
{
  val IR: DeliteOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DBooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case DEqual(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
    case DNotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
//    case DIntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
//    case DIntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
//    case DIntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
//    case DIntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
//    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
//    case DLessThan(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
//    case DGreaterThan(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
//    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")

    case DIntPlus(lhs,rhs) => emitValDef(sym, "clb_add(32)")
    case DIntMinus(lhs,rhs) => emitValDef(sym, "clb_sub(32)")
    case DIntTimes(lhs,rhs) => emitValDef(sym, "clb_mul(32)")
    case DIntDivide(lhs,rhs) => emitValDef(sym, "clb_div(32)")
//    case DLessThan(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
//    case DGreaterThan(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
//    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
//    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}

trait HwGenDeliteArrayOps extends HwCodegen
{
}


/*
 * LMS traits
 * The following traits are usually defined in LMS for the other code
 * generators. I've defined them all in Delite here
 */
trait HwGenOrderingOps extends HwCodegen
{
}

trait HwGenVariables extends HwCodegen
{
}

trait HwGenWhile extends HwCodegen
{
}

trait HwGenRangeOps extends HwCodegen
{
}

trait HwGenArrayOps extends HwCodegen
{
}

trait HwGenBooleanOps extends HwCodegen
{
}

trait HwGenPrimitiveOps extends HwCodegen
{
//  val IR: PrimitiveOpsExp
//  import IR._
//
//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
//    rhs match {
//      case ObjDoublePositiveInfinity() => emitValDef(sym, "INFINITY")
//      case ObjDoubleNegativeInfinity() => emitValDef(sym, "-INFINITY")
//      case _ => super.emitNode(sym, rhs)
//    }
//  }
}

trait HwGenObjectOps extends HwCodegen
{
}
