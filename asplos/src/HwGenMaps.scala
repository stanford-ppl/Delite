package asplos
import asplos._

import ppl.delite.framework.codegen.hw.HwCodegen
import scala.collection.mutable.Map
import scala.collection.mutable.Set

trait HwGenMaps extends HwCodegen {
  val IR: PPLOpsExp
  import IR._

  // If you see _1, emit _2
  var aliasMap = Map[Exp[Any], Exp[Any]]()
  // If you see 'Exp' while codegenerating for Loop 'Sym', prepend 'String' to 'Exp'
  var arbitPrefixMap = Map[(Sym[Any], Exp[Any]), String]()
  // dbl buf writer -> numrports map used during DeliteArray*
  var dblBufMap = Map[Sym[Any], Int]()

  // Maps to handle array2D structs
  var reverseStructMap = Map[Exp[Any],Exp[Any]]()
  var structInfoMap = Map[(Exp[Any],String), Exp[Any]]()
  val validMemorySet = Set[Exp[Any]]()


  // Maintain a map of all symbols corresponding to memories
  // which when encountered in DeliteArrayApply, we should emit
  // a read from the value Exp (also a memory location), and use
  // "String" to choose between the two memory locations as the
  // final result.
  // So far has one use case - select between brought in accumulator
  // and existing accumulator for reduction
  val memoryMuxMap = Map[Exp[Any], (Exp[Any], String)]()

  override def quote(x: Exp[Any]) = {
    var found = false
    var i = 0
    var retStr = ""

    while (!found && (i < curSym.length)) {
      if (arbitPrefixMap.contains((curSym(i), x))) {
        retStr = arbitPrefixMap((curSym(i),x)) + "_" + super.quote(x)
        found = true
      }
      i += 1
    }

    if (!found) {
      retStr = super.quote(x)
    }

    retStr

//    arbitPrefixMap.getOrElse((curSym.top, x), "") + super.quote(x)
  }

  protected def findConst(e: Exp[Any]) : Const[Int] = {
    e match {
      case c: Const[Int] => c
      case s: Sym[Int] =>
        val aliasS = aliasMap.getOrElse(s,s).asInstanceOf[Sym[Any]]
        val d = getdef(aliasS)
        d match {
          case MathMin(v1, v2) =>
            if (v1.isInstanceOf[Const[Int]]) {
              v1.asInstanceOf[Const[Int]]
            } else if (v2.isInstanceOf[Const[Int]]) {
              v2.asInstanceOf[Const[Int]]
            } else if (v1.isInstanceOf[Tunable]) {
              Const(v1.asInstanceOf[Tunable].value.get)  // Assuming that tunables are set by now
            } else if (v2.isInstanceOf[Tunable]) {
              Const(v2.asInstanceOf[Tunable].value.get)  // Assuming that tunables are set by now
            } else {
              sys.error(s"No const found for expression $e")
              Const(-1)
            }
          case DIntTimes(v1, v2) if (v1.isInstanceOf[Const[Int]] && v2.isInstanceOf[Const[Int]]) =>
              val v1Const = v1.asInstanceOf[Const[Int]]
              val v2Const = v2.asInstanceOf[Const[Int]]
              Const(v1Const.x * v2Const.x)
          case DIntTimes(v1, v2) if (v1.isInstanceOf[Tunable] && v2.isInstanceOf[Tunable]) =>
              val v1T = v1.asInstanceOf[Tunable]
              val v2T = v2.asInstanceOf[Tunable]
              Const(v1T.value.get * v2T.value.get)
          case DIntTimes(v1,v2) =>
            val c1:Const[Int] = findConst(v1)
            val c2:Const[Int] = findConst(v2)
            Const(c1.x * c2.x)
          case _ =>
            sys.error(s"No idea how to find consts, $s, $d")
            Const(-1)
        }
      case _ =>
        sys.error(s"$e is neither a sym nor a const. What is it?")
        Const(-1)
    }
  }
}
