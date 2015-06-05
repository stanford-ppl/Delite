package ppl.delite.framework.ops

import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
import ppl.delite.framework.Config
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.CCodegen
import java.io.{StringWriter, PrintWriter}


trait BaseDeliteOpsTraversalFat extends BaseLoopsTraversalFat {
  val IR: DeliteOpsExp
  import IR._

  /*
    // overridden only to attach DeliteFatOp trait to result ...
    override def fatten(e: TP[Any]): TTP = e.rhs match {
      case op: DeliteOpLoop[_] =>
        TTP(List(e.sym), DeliteFatLoop(op.size, op.v, List(op.body)))
      case _ => super.fatten(e)
    }
  */

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case e: DeliteCollectElem[_,_,_] if e.cond.isEmpty => Some(e.func.res)
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
  //    case e: DeliteReduceElem[_] => Some((e.func, e.cond)) // TODO: aks -- testing fusing conditionals for reduce elems
    case e: DeliteHashReduceElem[_,_,_,_] => Some((e.valFunc.res, e.cond.map(_.res))) // FIXME: HACK!!
    case e: DeliteCollectElem[_,_,_] => Some((e.func.res, e.cond.map(_.res)))
    case _ => super.unapplySimpleCollectIf(e)
  }

  // FIXME: need to modify .par from ParPlat to ParBuf accordingly
  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match {
    case e: DeliteHashCollectElem[_,_,_,_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mV,e.mI,e.mCV,e.mCI,e.mCCV)
    case e: DeliteHashReduceElem[_,_,_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mV,e.mI,e.mCV)
    case e: DeliteHashIndexElem[_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mK,e.mCV)
    case e: DeliteCollectElem[_,_,_] => e.copy(par = ParBuffer, cond = e.cond ++ c.map(Block(_)))(e.mA, e.mI, e.mCA)
    case e: DeliteReduceElem[_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mA)
    case e: DeliteReduceTupleElem[_,_] => e.copy(cond = e.cond ++ c.map(Block(_)))(e.mA,e.mB)
    case _ => super.applyAddCondition(e,c)
  }

  override def canApplyAddCondition(e: Def[Any]): Boolean = e match {
    case e: DeliteForeachElem[_] => false
    case _ => super.canApplyAddCondition(e)
  }

  override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]) = Config.opfusionEnabled
}

trait BaseGenDeliteOps extends BaseDeliteOpsTraversalFat with BaseGenLoopsFat with LoopFusionOpt with BaseGenStaticData {
  val IR: DeliteOpsExp
  import IR._
  //abstract override def emitValDef(sym: Sym[Any], rhs: String): Unit =
  //  if (!simpleCodegen) super.emitValDef(sym,rhs) else stream.print(quote(sym)+";")
  //def emitVarDef(sym: Sym[Any], rhs: String): Unit
  //def quote(x: Exp[Any]) : String =

  // TODO: what about deliteResult and deliteInput??

  // CAVEAT: DeliteCodegen does not inherit from this trait, so this is called
  // only within kernels

  override def focusBlock[A](result: Block[Any])(body: => A): A = {
    var saveKernel = deliteKernel
    deliteKernel = false
    val ret = super.focusBlock(result)(body)
    deliteKernel = saveKernel
    ret
  }

  // We don't override quote because we should not convert all Int sites,
  // but only particular ones (e.g. variable instantation).
  def quoteWithPrecision(x: Exp[Any]) = x match {
    case Const(i: Int) if Config.intSize == "long" => i.toString + "L"
    case _ => quote(x)
  }
}

/* CPU-like target code generation for DeliteOps */
trait GenericGenDeliteOps extends BaseGenLoopsFat with BaseGenStaticData with BaseGenDeliteOps with DeliteKernelCodegen {
  import IR._

  def quotearg(x: Sym[Any])
  def quotetp(x: Sym[Any])
  def methodCall(name:String, inputs: List[String] = Nil): String
  def emitMethodCall(name:String, inputs: List[String]): Unit
  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit): Unit
  def createInstance(typeName:String, args: List[String] = Nil): String
  def fieldAccess(className: String, varName: String): String
  def releaseRef(varName: String): Unit
  def emitReturn(rhs: String)
  def emitFieldDecl(name: String, tpe: String)
  def emitClass(name: String)(body: => Unit)
  def emitObject(name: String)(body: => Unit)
  def emitValDef(name: String, tpe: String, init: String): Unit
  def emitVarDef(name: String, tpe: String, init: String): Unit
  def emitAssignment(name: String, tpe: String, rhs: String): Unit
  def emitAssignment(lhs: String, rhs: String): Unit
  def emitAbstractFatLoopHeader(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit
  def emitAbstractFatLoopFooter(syms: List[Sym[Any]], rhs: AbstractFatLoop): Unit
  def emitHeapMark(): Unit
  def emitHeapReset(result: List[String]): Unit
  def castInt32(name: String): String
  def refNotEq: String
  def nullRef: String
  def arrayType(argType: String): String
  def arrayApply(arr: String, idx: String): String
  def newArray(argType: String, size: String): String
  def hashmapType(argType: String): String
  def typeCast(sym: String, to: String): String
  def withBlock(name: String)(block: => Unit): Unit

  // Because emitMethod and emitMethodCall are being overridden to pass additional runtime arguments now,
  // we need these "clean" versions for certain cases (e.g. serialization). This is ugly, and should be unified.
  def unalteredMethodCall(name:String, inputs: List[String] = Nil): String = methodCall(name, inputs)
  def emitUnalteredMethodCall(name:String, inputs: List[String]) = emitMethodCall(name, inputs)
  def emitUnalteredMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) = emitMethod(name, outputType, inputs)(body)

  def syncType(actType: String): String
  def emitWorkLaunch(kernelName: String, rSym: String, allocSym: String, syncSym: String): Unit

  // variable for loop nest level (used for adding openmp pragma)
  var loopLevel: Int = -1

  //marks alloc calls that create the outputs of the kernel
  var kernelAlloc = false
  def allocGlobal[T](block: => Unit) {
    kernelAlloc = true
    block
    kernelAlloc = false
  }

  abstract class GCStrategy
  case object Iteration extends GCStrategy //free every iteration (know nothing escapes but the alloc)
  case object Global extends GCStrategy //free at the end of the loop (cross-iteration dependencies, can track how the output escapes and copy)
  case object Unknown extends GCStrategy //free nothing (don't know what escapes)

  def gcStrategy[A](body: Def[A]): GCStrategy = body match {
    // case b:DeliteCollectElem[_,_,_] => if (isPrimitiveType(b.mA)) Iteration else Unknown
    // case b:DeliteReduceElem[_] => if (isPrimitiveType(b.mA)) Iteration else Global
    // case b:DeliteReduceTupleElem[_,_] => if (isPrimitiveType(b.mA) && isPrimitiveType(b.mB)) Iteration else Global
    // case b:DeliteForeachElem[_] => Unknown
    // case b:DeliteHashCollectElem[_,_,_,_,_,_] => Unknown //TODO: handle inner collection allocations specially
    // case b:DeliteHashReduceElem[_,_,_,_] => if (isPrimitiveType(b.mV)) Iteration else Unknown
    // case b:DeliteHashIndexElem[_,_] => Unknown //TODO: allocate the hashmap specially iff it's returned and ignore this?
    case _ => Unknown
  }

  //allocations of horizontally fused loops likely to be interleaved in the heap, so we need to take the most conservative strategy
  def gcStrategy(op: AbstractFatLoop): GCStrategy = {
    def join(lhs: GCStrategy, rhs: GCStrategy) = {
      if (lhs == Unknown || rhs == Unknown) Unknown
      else if (lhs == Global || rhs == Global) Global
      else Iteration
    }
    op.body.map(b => gcStrategy(b)).reduce(join)
  }

  def gcSyms(symList: List[Sym[Any]], op: AbstractFatLoop) = {
    for ((sym, body) <- (symList zip op.body) if (gcStrategy(body) == Global)) yield sym
  }

  /**
   * MultiLoop components
   */

  /* (grouped) hash support follows */
  def emitInlineMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) { //TODO: to properly abstract this over multiple code generators we need a hashmap impl for each!
        emitVarDef(kps.map(p=>quote(p._1)).mkString("") + "_hash_pos", hashmapType(remap(getBlockResult(key).tp)), createInstance(hashmapType(remap(getBlockResult(key).tp)), List("512","128")))
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128")
            emitBlock(elem.buf.alloc)
            emitVarDef(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128")
            emitBlock(elem.buf.alloc)
            emitVarDef(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
      }
    }
  }

  def emitKernelMultiHashDecl(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], actType: String) {
    if (ps.length > 0) {
      emitFieldDecl("all_acts", arrayType(actType))
      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          emitFieldDecl(quotedGroup + "_hash_pos", hashmapType(remap(getBlockResult(key).tp)))
          emitFieldDecl(quotedGroup + "_size", remap(Manifest.Int))
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitFieldDecl(quote(sym), remap(sym.tp))
              emitFieldDecl(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp))
              emitFieldDecl(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp))
              emitMethod(quote(sym)+"_data_set", remap(Manifest.Unit), List(("xs",remap(elem.buf.allocVal.tp)))) {
                emitAssignment(quote(sym) + "_data", "xs")
                stream.println("if (left_act " + refNotEq + " " + nullRef + ")")
                emitMethodCall(fieldAccess("left_act",quote(sym)+"_data_set"),List("xs")) // XX linked frame
              }
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              emitFieldDecl(quote(sym), remap(sym.tp))
              emitFieldDecl(quote(sym) + "_hash_data", remap(getBlockResult(elem.buf.alloc).tp))
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
              emitFieldDecl(quote(sym), remap(sym.tp))
          }
        }
      }
    }
  }

  def emitKernelMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = ""){
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
        val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
        emitAssignment(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), createInstance(hashmapType(remap(getBlockResult(key).tp)), List("512","128")))
        emitAssignment(fieldAccess(prefixSym, quotedGroup + "_size"), "-1")
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
            emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128")
            emitBlock(elem.buf.alloc)
            emitAssignment(fieldAccess(prefixSym, quote(sym) + "_hash_data"), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "128")
            emitBlock(elem.buf.alloc)
            emitAssignment(fieldAccess(prefixSym, quote(sym) + "_hash_data"), quote(getBlockResult(elem.buf.alloc)))
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
      }
    }
  }

  def emitMultiHashElem(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    for ((cond,cps) <- ps.groupBy(_._2.cond)) { // group by cond
      if (cond.nonEmpty) stream.println("if (" + cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) { // group by key
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          stream.println("// common key "+key+" for "+quotedGroup)
          emitValDef(quotedGroup + "_sze", remap(manifest[Int]), fieldAccess(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), unalteredMethodCall("size")))
          emitValDef(quotedGroup + "_idx", remap(manifest[Int]), fieldAccess(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), "put(" + quote(getBlockResult(key)) + ")"))

          stream.println("if (" + quotedGroup + "_idx == " + quotedGroup + "_sze) { // new key")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              //allocate inner buffer & append to outer buffer
              emitValDef(quote(elem.iBuf.sV), remap(elem.buf.sV.tp), "128")
              emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_hash_data"), quote(elem.buf.allocVal))
              getActSize = quotedGroup + "_sze"
              if (!Config.soaEnabled) {
                emitBlock(elem.iBuf.alloc)
                emitValDef(quote(elem.iBuf.allocVal), remap(getBlockResult(elem.iBuf.alloc).tp), quote(getBlockResult(elem.iBuf.alloc)))
              }
              emitBlock(elem.buf.append)
              //append elem to inner buffer
              emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), getActSize)
              emitValDef(quote(elem.iBuf.eV), remap(elem.iBuf.eV.tp), quote(getBlockResult(elem.valFunc)))
              emitBlock(elem.iBuf.append)
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.valFunc)))
              getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              getActSize = quotedGroup + "_sze"
              emitBlock(elem.buf.append)
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }

          stream.println("} else { // existing key")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "_idx")
              emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              emitValDef(quote(elem.iBuf.eV), remap(elem.iBuf.eV.tp), quote(getBlockResult(elem.valFunc)))
              if (!Config.soaEnabled) {
                emitBlock(elem.buf.apply)
                emitValDef(quote(elem.iBuf.allocVal), remap(getBlockResult(elem.buf.apply).tp), quote(getBlockResult(elem.buf.apply)))
              }
              emitBlock(elem.iBuf.append) //append to inner buffer
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              if (elem.rFunc != Block(elem.rV._1)) { //else drop
                emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "_idx")
                emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
                if (elem.rFunc == Block(elem.rV._2)) { //overwrite
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.valFunc)))
                  emitBlock(elem.buf.update)
                } else { //reduce
                  emitBlock(elem.buf.apply)
                  emitValDef(quote(elem.rV._1), remap(elem.rV._1.tp), quote(getBlockResult(elem.buf.apply)))
                  emitValDef(quote(elem.rV._2), remap(elem.rV._2.tp), quote(getBlockResult(elem.valFunc)))
                  emitBlock(elem.rFunc)
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.rFunc)))
                  emitBlock(elem.buf.update)
                }
              }
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
          stream.println("}")
        }
      if (cond.nonEmpty) stream.println("}")
    }
  }

  def emitMultiHashCombine(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String){
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
        val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
        stream.println("// common key "+key+" for "+quotedGroup)
        stream.print("if (" + fieldAccess(prefixSym, quotedGroup+"_size") + " == -1) ") //store pre-merge sizes for later
        emitAssignment(fieldAccess(prefixSym, quotedGroup + "_size"), fieldAccess(prefixSym, fieldAccess(quotedGroup+"_hash_pos",unalteredMethodCall("size"))))
        stream.print("if (" + fieldAccess("rhs", quotedGroup+"_size") + " == -1) ")
        emitAssignment(fieldAccess("rhs", quotedGroup+"_size"), fieldAccess("rhs", fieldAccess(quotedGroup+"_hash_pos",unalteredMethodCall("size"))))
        emitVarDef(quotedGroup + "rhs_idx", remap(manifest[Int]), "0")
        stream.println("while (" + quotedGroup + "rhs_idx < " + fieldAccess("rhs", fieldAccess(quotedGroup+"_hash_pos",unalteredMethodCall("size"))) + ") {")
        emitValDef(quotedGroup + "_k", remap(key.tp), fieldAccess("rhs", fieldAccess(quotedGroup + "_hash_pos",arrayApply(unalteredMethodCall("unsafeKeys"),quotedGroup + castInt32("rhs_idx")))))
        emitValDef(quotedGroup + "_sze", remap(manifest[Int]), fieldAccess(prefixSym, fieldAccess(quotedGroup + "_hash_pos",unalteredMethodCall("size"))))
        emitValDef(quotedGroup + "_idx", remap(manifest[Int]), fieldAccess(prefixSym, fieldAccess(quotedGroup + "_hash_pos","put("+quotedGroup+"_k)")))
        stream.println("if (" + quotedGroup + "_idx == " + quotedGroup + "_sze) { // new key")
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess("rhs", quote(sym) + "_hash_data"))
            emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "rhs_idx")
            emitBlock(elem.buf.apply)
            emitAssignment(quote(elem.buf.allocVal), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
            emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.buf.apply)))
            getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_hash_data"))
            getActSize = quotedGroup + "_sze"
            emitBlock(elem.buf.append)
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
        stream.println("} else { // existing key")
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            if (elem.rFunc != Block(elem.rV._1)) { //else drop
              emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess("rhs", quote(sym) + "_hash_data"))
              emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "rhs_idx")
              emitBlock(elem.buf.apply)
              emitValDef(quote(sym) + "_v", remap(getBlockResult(elem.buf.apply).tp), quote(getBlockResult(elem.buf.apply)))
              withBlock(quote(sym) + "_reduce_block") {
                emitValDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
                emitValDef(quote(elem.buf.iV), remap(elem.buf.iV.tp), quotedGroup + "_idx")
                if (elem.rFunc == Block(elem.rV._2)) { //overwrite
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(sym) + "_v")
                  emitBlock(elem.buf.update)
                } else { //reduce
                  emitBlock(elem.buf.apply)
                  emitValDef(quote(elem.rV._1), remap(elem.rV._1.tp), quote(getBlockResult(elem.buf.apply)))
                  emitValDef(quote(elem.rV._2), remap(elem.rV._2.tp), quote(sym)+"_v")
                  emitBlock(elem.rFunc)
                  emitValDef(quote(elem.buf.eV), remap(elem.buf.eV.tp), quote(getBlockResult(elem.rFunc)))
                  emitBlock(elem.buf.update)
                }
              }
            }
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
        }
        stream.println("}")
        emitAssignment(quotedGroup + "rhs_idx", quotedGroup + "rhs_idx+1")
        stream.println("}") // end loop
      }
    }
  }

  def emitMultiHashPostCombine(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String){ }

  def emitMultiHashPostProcInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], actType: String, prefixSym: String){
    if (ps.length > 0) {
      emitValDef("tid", remap(Manifest.Int), fieldAccess(resourceInfoSym,"groupId"))
      stream.println("if (tid > 0) {")
      emitValDef("all_acts", arrayType(actType), newArray(actType, fieldAccess(resourceInfoSym, "groupSize")))
      emitVarDef("currentAct", actType, prefixSym)
      emitVarDef("i", remap(manifest[Int]), "tid")
      stream.println("while(i >= 0) {")
      emitAssignment(arrayApply("all_acts",castInt32("i")), "currentAct")
      emitAssignment(fieldAccess("currentAct","all_acts"), "all_acts")
      emitAssignment("currentAct", fieldAccess("currentAct","left_act"))
      emitAssignment("i", "i-1")
      stream.println("}")

      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitValDef(elem.buf.sV, fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"),unalteredMethodCall("size")))
              emitValDef(elem.buf.allocVal, fieldAccess(prefixSym, quote(sym)+"_hash_data"))
              emitBlock(elem.buf.allocRaw)
              emitMethodCall(fieldAccess(prefixSym,quote(sym)+"_data_set"),List(quote(getBlockResult(elem.buf.allocRaw))))
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
        }
      }
      stream.println("} else {")
      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
              emitMethodCall(fieldAccess(prefixSym,quote(sym)+"_data_set"),List(fieldAccess(prefixSym,quote(sym)+"_hash_data")))
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
        }
      }
      stream.println("}")
    }
  }

  def emitMultiHashPostProcess(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], actType: String, prefixSym: String){
    if (ps.length > 0) {
      emitValDef("tid", remap(Manifest.Int), fieldAccess(resourceInfoSym, "groupId"))
      emitValDef("numThreads", remap(Manifest.Int), fieldAccess(resourceInfoSym, "groupSize"))
      stream.println("if (" + fieldAccess(prefixSym,"all_acts") + " " + refNotEq + " " + nullRef + ") {")
      emitValDef("all_acts", arrayType(actType), fieldAccess(prefixSym,"all_acts"))
      for ((cond,cps) <- ps.groupBy(_._2.cond)) {
        for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
          val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
          emitValDef(quotedGroup+"_globalKeys", arrayType(remap(key.tp)), fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"), unalteredMethodCall("unsafeKeys")))
          emitVarDef(quotedGroup+"_idx", remap(Manifest.Int), typeCast(typeCast(fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"), unalteredMethodCall("size")), remap(Manifest.Long)) + " * tid / numThreads",remap(Manifest.Int)))
          emitValDef(quotedGroup+"_end", remap(Manifest.Int), typeCast(typeCast(fieldAccess(fieldAccess(fieldAccess(prefixSym, arrayApply("all_acts","0")), quotedGroup+"_hash_pos"), unalteredMethodCall("size")), remap(Manifest.Long)) + " * (tid+1) / numThreads",remap(Manifest.Int)))
          stream.println("while (" + quotedGroup+"_idx < " + quotedGroup + "_end) {")
          kps foreach {
            case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) =>
                emitValDef(elem.buf.iV, quotedGroup+"_idx")
                emitValDef(elem.buf.allocVal, fieldAccess(prefixSym,quote(sym)+"_data"))
                emitVarDef(quote(sym)+"_act_idx", remap(Manifest.Int), "0")
                emitVarDef(quote(sym)+"_values_size", remap(Manifest.Int), "0")
                stream.println("while (" + quote(sym)+"_act_idx < numThreads) {")
                  emitValDef("currentAct", actType, arrayApply("all_acts",quote(sym)+castInt32("_act_idx")))
                  emitValDef("pos", remap(Manifest.Int), fieldAccess(fieldAccess("currentAct", quotedGroup+"_hash_pos"), "get("+arrayApply(quotedGroup+"_globalKeys",castInt32(quotedGroup+"_idx"))+")"))
                  stream.println("if (pos != -1 && pos < " + fieldAccess("currentAct",quotedGroup+"_size") + ") {")
                    emitValDef(elem.buf.iV2, "pos")
                    emitValDef(elem.buf.aV2, fieldAccess("currentAct",quote(sym)+"_hash_data"))
                    if (!Config.soaEnabled) {
                      emitBlock(elem.iBuf.allocRaw)
                      emitValDef(quote(elem.iBuf.aV2), remap(getBlockResult(elem.iBuf.allocRaw).tp), quote(getBlockResult(elem.iBuf.allocRaw)))
                    }
                    emitBlock(elem.iBufSize)
                    emitAssignment(quote(sym)+"_values_size", quote(sym)+"_values_size + " + quote(getBlockResult(elem.iBufSize)))
                  stream.println("}")
                  emitAssignment(quote(sym)+"_act_idx", quote(sym)+"_act_idx + 1")
                stream.println("}")

                emitValDef(elem.buf.sV, quote(sym)+"_values_size")
                if (!Config.soaEnabled) {
                  emitBlock(elem.iBuf.alloc)
                  emitValDef(quote(elem.iBuf.allocVal), remap(getBlockResult(elem.iBuf.alloc).tp), quote(getBlockResult(elem.iBuf.alloc)))
                }
                emitBlock(elem.buf.update)

                emitVarDef(quote(sym)+"_offset", remap(Manifest.Int), "0")
                emitAssignment(quote(sym)+"_act_idx", "0")
                stream.println("while (" + quote(sym)+"_act_idx < numThreads) {")
                  emitValDef("currentAct", actType, arrayApply("all_acts",castInt32(quote(sym)+"_act_idx")))
                  emitValDef("pos", remap(Manifest.Int), fieldAccess(fieldAccess("currentAct", quotedGroup+"_hash_pos"), "get("+arrayApply(quotedGroup+"_globalKeys",castInt32(quotedGroup+"_idx"))+")"))
                  stream.println("if (pos != -1 && pos < " + fieldAccess("currentAct",quotedGroup+"_size") + ") {")
                    emitValDef(elem.buf.iV2, "pos")
                    emitValDef(elem.buf.aV2, fieldAccess("currentAct",quote(sym)+"_hash_data"))
                    if (!Config.soaEnabled) {
                      emitBlock(elem.iBuf.allocRaw)
                      emitValDef(quote(elem.iBuf.aV2), remap(getBlockResult(elem.iBuf.allocRaw).tp), quote(getBlockResult(elem.iBuf.allocRaw)))
                    }
                    emitBlock(elem.iBufSize)
                    emitValDef(elem.iBuf.sV, quote(getBlockResult(elem.iBufSize)))
                    emitValDef(elem.iBuf.iV, quote(sym)+"_offset")
                    emitValDef(elem.iBuf.iV2, "0")
                    emitBlock(elem.iBuf.copyRaw)
                    emitAssignment(quote(sym)+"_offset", quote(sym)+"_offset + " + quote(getBlockResult(elem.iBufSize)))
                  stream.println("}")
                  emitAssignment(quote(sym)+"_act_idx", quote(sym)+"_act_idx + 1")
                stream.println("}")
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
            case (sym, elem: DeliteHashIndexElem[_,_]) =>
          }
          emitAssignment(quotedGroup+"_idx", quotedGroup+"_idx + 1")
          stream.println("}")
        }
      }
      stream.println("}")
    }
  }

  def emitMultiHashFinalize(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    for ((cond,cps) <- ps.groupBy(_._2.cond)) {
      for ((key,kps) <- cps.groupBy(_._2.keyFunc)) {
        val quotedGroup = kps.map(p=>quote(p._1)).mkString("")
        emitValDef(quotedGroup + "_sze", remap(manifest[Int]), fieldAccess(fieldAccess(prefixSym, quotedGroup + "_hash_pos"), unalteredMethodCall("size")))
        kps foreach {
          case (sym, elem: DeliteHashCollectElem[_,_,_,_,_,_]) => //TODO: finalizer
            if (prefixSym == "") {
              emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
              getActBuffer = List(quote(elem.buf.allocVal))
              emitAssignment(quote(elem.buf.sV), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitValDef(quote(sym), remap(sym.tp), quote(elem.buf.allocVal))
            }
            else {
              emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_data"))
              getActBuffer = List(quote(elem.buf.allocVal))
              emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitAssignment(fieldAccess(prefixSym, quote(sym)), quote(elem.buf.allocVal))
            }
          case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => //TODO: finalizer
            emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess(prefixSym, quote(sym) + "_hash_data"))
            getActBuffer = List(quote(elem.buf.allocVal))
            if (prefixSym == "") {
              emitAssignment(quote(elem.buf.sV), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitValDef(quote(sym), remap(sym.tp), quote(elem.buf.allocVal))
            }
            else {
              emitValDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quotedGroup + "_sze")
              emitBlock(elem.buf.setSize)
              emitAssignment(fieldAccess(prefixSym, quote(sym)), quote(elem.buf.allocVal))
            }
          case (sym, elem: DeliteHashIndexElem[_,_]) =>
            if (prefixSym == "")
              emitValDef(quote(sym), remap(sym.tp), quotedGroup + "_hash_pos")
            else
              emitAssignment(fieldAccess(prefixSym, quote(sym)), fieldAccess(prefixSym, quotedGroup + "_hash_pos"))
        }
      }
    }
  }

  // --- end hash reduce

  def emitCollectElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteCollectElem[_,_,_], prefixSym: String = "") {
    elem.par match {
      case ParBuffer | ParSimpleBuffer =>
        if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        if (elem.iFunc.nonEmpty) {
          emitValDef(elem.eF.get, quote(getBlockResult(elem.iFunc.get)))
          emitBlock(elem.sF.get)
          emitVarDef(quote(elem.iF.get), remap(elem.iF.get.tp), "0")
          stream.println("while (" + quote(elem.iF.get) + " < " + quote(getBlockResult(elem.sF.get)) + ") { //flatMap loop")
          emitBlock(elem.func)
        }
        emitValDef(elem.buf.allocVal, fieldAccess(prefixSym,quote(sym)+"_buf"))
        emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
        getActBuffer = List(fieldAccess(prefixSym, quote(sym) + "_buf"))
        getActSize = fieldAccess(prefixSym, quote(sym) + "_size")
        emitBlock(elem.buf.appendable)
        stream.println("if (" + quote(getBlockResult(elem.buf.appendable)) + ") {")
        emitBlock(elem.buf.append)
        emitAssignment(fieldAccess(prefixSym,quote(sym)+"_size"), fieldAccess(prefixSym,quote(sym)+"_size") + " + 1")
        stream.println("}")
        emitAssignment(fieldAccess(prefixSym,quote(sym)+"_conditionals"), fieldAccess(prefixSym,quote(sym)+"_conditionals") + " + 1")
        if (elem.iFunc.nonEmpty) {
          emitAssignment(quote(elem.iF.get), quote(elem.iF.get) + " + 1")
          stream.println("}") //close flatmap loop
        }
        if (elem.cond.nonEmpty) stream.println("}")
      case ParFlat =>
        emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
        emitValDef(elem.buf.allocVal, fieldAccess(prefixSym,quote(sym)+"_data"))
        if (elem.cond.nonEmpty) {
          stream.println("//ERROR: need to test for conds " + elem.cond)
          println("ERROR: need to test for conds " + elem.cond)
        }
        emitBlock(elem.buf.update)
    }
  }

  var getActSize = ""
  var getActBuffer: List[String] = Nil

  def emitForeachElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteForeachElem[_]) {
    emitAssignment(quote(sym), remap(sym.tp), quote(getBlockResult(elem.func)))
    //stream.println(quote(getBlockResult(elem.func)))
  }

  // -- begin emit reduce

  // Assigns the first reduce element to the result symbol
  def emitFirstReduceElemAssign(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    val resultSym = fieldAccess(prefixSym,quote(sym))
    if (elem.cond.nonEmpty) {
      // if we have conditionals, we have to delay the the initialization of the accumulator to the
      // first element where the condition is true
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
      emitAssignment(resultSym,quote(getBlockResult(elem.func)))
      stream.println("} else {")
      emitAssignment(resultSym,fieldAccess(prefixSym,quote(sym)+"_zero"))
      stream.println("}")
    }
    else {
      emitAssignment(resultSym,quote(getBlockResult(elem.func)))
    }
  }

  def emitReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {"/*}*/)
      if (elem.stripFirst)
        emitInitializeOrReduction(op, sym, elem, prefixSym)
      else
        emitReduction(op, sym, elem, prefixSym)
      stream.println("}")
    }
    else {
      emitReduction(op, sym, elem, prefixSym)
    }
  }

  def emitReduceTupleElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {"/*}*/)
      assert(!elem.stripFirst, "tuple reduce with condition + stripFirst not implemented")
      emitReductionTuple(op, sym, elem, prefixSym)
      stream.println("}")
    }
    else {
      emitReductionTuple(op, sym, elem, prefixSym)
    }
  }

  def emitInitializeOrReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    stream.println("// TODO: we could optimize this check away with more convoluted runtime support if necessary")
    stream.println("if (" + fieldAccess(prefixSym,quote(sym)) + " == " + fieldAccess(prefixSym,quote(sym)+"_zero") + ") ")

    // initialize
    emitAssignment(fieldAccess(prefixSym,quote(sym)),quote(getBlockResult(elem.func)))

    // or reduce
    stream.println("else {")
    emitReduction(op, sym, elem, prefixSym)
    stream.println("}")
  }

  def emitReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    emitValDef(elem.rV._1, fieldAccess(prefixSym,quote(sym)))
    emitValDef(elem.rV._2, quote(getBlockResult(elem.func)))
    emitBlock(elem.rFunc)
    emitAssignment(fieldAccess(prefixSym,quote(sym)), quote(getBlockResult(elem.rFunc)))
  }

  def emitReductionTuple(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String) {
    val rV = elem.rVSeq
    val rFunc = elem.rFuncSeq
    emitValDef(rV._1._1, fieldAccess(prefixSym,quote(sym)))
    emitValDef(rV._1._2, fieldAccess(prefixSym,quote(sym)+"_2"))
    emitValDef(rV._2._1, quote(getBlockResult(elem.func._1)))
    emitValDef(rV._2._2, quote(getBlockResult(elem.func._2)))
    emitFatBlock(List(rFunc._1, rFunc._2))
    emitAssignment(fieldAccess(prefixSym,quote(sym)), quote(getBlockResult(rFunc._1)))
    emitAssignment(fieldAccess(prefixSym,quote(sym)+"_2"), quote(getBlockResult(rFunc._2)))
  }

  // -- end emit reduce emit

  def getMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) = op.body flatMap { // don't emit dependencies twice!
    case elem: DeliteHashCollectElem[_,_,_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
    case elem: DeliteHashReduceElem[_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
    case elem: DeliteHashIndexElem[_,_] => elem.keyFunc :: elem.cond
    case elem: DeliteCollectElem[_,_,_] if elem.iFunc.nonEmpty => elem.iFunc.get :: elem.cond
    case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
    //case elem: DeliteForeachElem[_] => elem.cond // only emit func inside condition! TODO: how to avoid emitting deps twice? // elem.func :: elem.cond
    case elem: DeliteForeachElem[_] => List(elem.func)
    case elem: DeliteReduceElem[_] => elem.func :: elem.cond
    case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
  }

  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // FIXME: without .distinct TPCHQ2 has duplicate definitions. this should be fixed in emitFatBlock.
    emitFatBlock(getMultiLoopFuncs(op, symList).distinct)
  }

  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    emitInlineMultiHashInit(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        elem.par match {
          case ParBuffer | ParSimpleBuffer =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), "0")
          case ParFlat =>
            emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quote(op.size))
        }
        emitBlock(elem.buf.alloc)
        elem.par match {
          case ParBuffer | ParSimpleBuffer =>
            emitVarDef(quote(sym) + "_buf", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          case ParFlat =>
            emitValDef(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
        }
        emitVarDef(quote(sym) + "_size", remap(Manifest.Int), "0")
        emitVarDef(quote(sym) + "_conditionals", remap(Manifest.Int), "0")
      case (sym, elem: DeliteHashElem[_,_]) => //done above
      case (sym, elem: DeliteForeachElem[_]) =>
        emitVarDef(quote(sym), remap(sym.tp), "()")  //TODO: Need this for other targets? (Currently, other targets just don't generate unit types)
      case (sym, elem: DeliteReduceElem[_]) =>
        emitBlock(elem.zero)
        emitValDef(quote(sym) + "_zero", remap(sym.tp), quote(getBlockResult(elem.zero)))
        emitVarDef(quote(sym), remap(sym.tp), quote(sym) + "_zero")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitBlock(elem.zero._1)
        emitValDef(quote(sym) + "_zero", remap(elem.zero._1.tp), quote(getBlockResult(elem.zero._1)))
        emitBlock(elem.zero._2)
        emitValDef(quote(sym) + "_zero_2", remap(elem.zero._2.tp), quote(getBlockResult(elem.zero._2)))
        /*stream.println("val " + quote(sym) + "_zero   = " + elem.zero._1)
        stream.println("val " + quote(sym) + "_zero_2 = " + elem.zero._2)*/
        emitVarDef(quote(sym), remap(elem.zero._1.tp), quote(sym) + "_zero")
        emitVarDef(quote(sym) + "_2", remap(elem.zero._2.tp), quote(sym) + "_zero_2")
    }
    //if (gcStrategy(op) == Global) emitHeapMark()
    emitVarDef(quote(op.v), remap(op.v.tp), "0")
    //if (true) { //op.body exists (loopBodyNeedsStripFirst _)) { preserve line count as indicator for succesful fusing
    if (op.body exists (loopBodyNeedsStripFirst _)) {
      stream.println("if (" + quote(op.size) + " > 0) { // prerun fat loop " + symList.map(quote).mkString(",")/*}*/)
      /* strip first iteration */
      emitMultiLoopFuncs(op, symList)
      emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitCollectElem(op, sym, elem)
        case (sym, elem: DeliteHashElem[_,_]) => // done above
        case (sym, elem: DeliteForeachElem[_]) =>
          emitForeachElem(op, sym, elem)
        case (sym, elem: DeliteReduceElem[_]) =>
          if (elem.stripFirst) {
            emitFirstReduceElemAssign(op, sym, elem)
          }
          else {
            emitReduceElem(op, sym, elem)
          }
      }
      stream.println(/*{*/"}")
      emitAssignment(quote(op.v), "1")
    }

    val freeVars = getFreeVarBlock(Block(Combine(getMultiLoopFuncs(op,symList).map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct
    val inputStreamVars = freeVars.filter(_.tp == manifest[DeliteFileInputStream])
    val outputStreamVars = freeVars.filter(_.tp == manifest[DeliteFileOutputStream])

    if (inputStreamVars.length > 0) {
      assert(inputStreamVars.length == 1, "ERROR: don't know how to handle multiple stream inputs at once")
      val streamSym = inputStreamVars(0)
      emitValDef(quote(streamSym)+"_stream", remap(streamSym.tp), fieldAccess(quote(streamSym),"openCopyAtNewLine(0)"))
      emitValDef(quote(streamSym)+"_offset", remap(manifest[Long]), fieldAccess(quote(streamSym), "streamOffset"))
      stream.println("while (" + fieldAccess(quote(streamSym)+"_stream", "position") + " < " + quote(streamSym) + "_offset + " + quote(op.size) + " ) {")
    }
    else {
      this match {
        case g: CCodegen if(loopLevel==1 && Config.debug && op.body.size==1) =>
          // only add openmp pragma to the outer-most loop in debug mode
          (symList zip op.body) foreach {
            case (sym, elem: DeliteCollectElem[_,_,_]) =>
              stream.println("#pragma omp parallel for")
            case (sym, elem: DeliteReduceElem[_]) =>
              //TODO: figure out the reduction operator.
              //stream.println("#pragma omp parallel for reduction(+:" + quote(sym) + ")")
            case _ => //
          }
          stream.println("for (int " + quote(op.v) + "=0; " + quote(op.v) + "<" + quote(op.size) + ";" + quote(op.v) + "++) {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
        case _ => stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(",")/*}*/)
      }
    }
    //if (gcStrategy(op) == Iteration) emitHeapMark()

    // body
    emitMultiLoopFuncs(op, symList)
    emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitCollectElem(op, sym, elem)
      case (sym, elem: DeliteHashElem[_,_]) => // done above
      case (sym, elem: DeliteForeachElem[_]) =>
        emitForeachElem(op, sym, elem)
      case (sym, elem: DeliteReduceElem[_]) =>
        emitReduceElem(op, sym, elem)
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitReduceTupleElem(op, sym, elem)
    }

    //if (gcStrategy(op) == Iteration) emitHeapReset(List())
    this match {
      case g: CCodegen if(loopLevel==1 && Config.debug && op.body.size==1) => //
      case _ => emitAssignment(quote(op.v), quote(op.v) + " + 1")
    }
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))

    if (inputStreamVars.length > 0) {
      stream.println(fieldAccess(quote(inputStreamVars(0))+"_stream","close();"))
    }

    if (outputStreamVars.length > 0) {
      val streamSyms = outputStreamVars.map(s => quote(s))
      streamSyms foreach { s => emitUnalteredMethodCall(s + ".close", Nil) }
    }

    // finalizer
    emitMultiHashFinalize(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) })
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        // if we are using parallel buffers, set the logical size of the output since it
        // might be different than the physically appended size for some representations
        if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
          emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), quote(sym) + "_buf")
          getActBuffer = List(quote(elem.buf.allocVal))
          emitAssignment(quote(elem.buf.sV), quote(sym) + "_conditionals")
          emitBlock(elem.buf.setSize)
        }
        else {
          emitValDef(elem.buf.allocVal, quote(sym) + "_data")
        }
        emitBlock(elem.buf.finalizer)
        emitValDef(sym, quote(getBlockResult(elem.buf.finalizer)))
      case (sym, elem: DeliteHashElem[_,_]) =>
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
    //if (gcStrategy(op) == Global) emitHeapReset(gcSyms(symList, op).map(quote))
  }

  def getKernelName(syms: List[Sym[Any]]) = syms.map(quote).mkString("")
  def getActType(kernelName: String) = "activation_"+kernelName

  def emitProcessLocal(actType: String) = {
    stream.println("//process local")
    emitValDef("tid", remap(Manifest.Int), fieldAccess(resourceInfoSym,"groupId"))
    emitValDef("numThreads", remap(Manifest.Int), fieldAccess(resourceInfoSym, "groupSize"))
    emitValDef("numChunks", remap(Manifest.Int), unalteredMethodCall(fieldAccess("sync", "numChunks"), List()))
    emitVarDef("dIdx", remap(Manifest.Int), "tid")
    stream.println("while (dIdx < numChunks) {")
      emitValDef("start", remap(Manifest.Long), "loopStart + loopSize*dIdx/numChunks")
      emitValDef("end", remap(Manifest.Long), "loopStart + loopSize*(dIdx+1)/numChunks")
      emitValDef("act", actType, methodCall("processRange", List("__act","start","end")))
      emitUnalteredMethodCall(fieldAccess("sync","set"), List("dIdx","act"))
      emitAssignment("dIdx", unalteredMethodCall(fieldAccess("sync","getNextChunkIdx"), List()))
    stream.println("}")
    emitValDef("localStart", remap(Manifest.Int), "tid*numChunks/numThreads")
    emitValDef("localEnd", remap(Manifest.Int), "(tid+1)*numChunks/numThreads")
    emitValDef("act", actType, unalteredMethodCall(fieldAccess("sync","get"), List("localStart")))
  }

  def emitCombineLocal() = {
    stream.println("//combine local")
    emitVarDef("i", remap(Manifest.Int), "localStart+1")
    stream.println("while (i < localEnd) {")
      emitMethodCall("combine", List("act", unalteredMethodCall(fieldAccess("sync","get"),List("i"))))
      emitAssignment("i", "i+1")
    stream.println("}")
  }

  def emitCombineRemote() = {
    stream.println("//combine remote")
    emitVarDef("half", remap(Manifest.Int), "tid")
    emitVarDef("step", remap(Manifest.Int), "1")
    stream.println("while ((half % 2 == 0) && (tid + step < numThreads)) {")
      emitMethodCall("combine", List("act", unalteredMethodCall(fieldAccess("sync","getC"), List("tid+step"))))
      emitAssignment("half", "half / 2")
      emitAssignment("step", "step * 2")
    stream.println("}")
    emitUnalteredMethodCall(fieldAccess("sync","setC"), List("tid","act"))
  }

  def emitPostCombine(actType: String) {
    stream.println("//post combine")
    stream.print("if (tid != 0) ")
    emitMethodCall("postCombine", List("act", unalteredMethodCall(fieldAccess("sync","getP"),List("tid-1"))))
    emitVarDef("j", remap(Manifest.Int), "localStart+1")
    emitVarDef("currentAct", actType, "act")
    stream.println("while (j < localEnd) {")
      emitValDef("rhsAct", actType, unalteredMethodCall(fieldAccess("sync","get"), List("j")))
      emitMethodCall("postCombine", List("rhsAct", "currentAct"))
      emitAssignment("currentAct", "rhsAct")
      emitAssignment("j", "j+1")
    stream.println("}")
    stream.print("if (tid == numThreads-1) ")
    emitMethodCall("postProcInit", List("currentAct"))
    emitUnalteredMethodCall(fieldAccess("sync","setP"), List("tid, currentAct"))
  }

  def emitPostProcess() {
    stream.println("//post process")
    emitVarDef("k", remap(Manifest.Int), "localStart")
    stream.println("while (k < localEnd) {")
      emitMethodCall("postProcess", List(unalteredMethodCall(fieldAccess("sync","get"),List("k"))))
      emitAssignment("k", "k+1")
    stream.println("}")
  }

  def emitFinalizer() {
    stream.print("if (tid == 0) ")
    emitMethodCall("finalize", List("act"))
  }

  def emitBarrier() {
    emitUnalteredMethodCall(fieldAccess("sync", "awaitBarrier"), List())
  }

  def emitKernelAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    // kernel mode
    val kernelName: String = getKernelName(symList)
    val actType = getActType(kernelName)
    //deliteKernel = false

    emitAbstractFatLoopHeader(symList, op)

    emitMethod("size", remap(Manifest.Long), Nil) { emitReturn(quote(op.size)) }

    emitFieldDecl("loopStart", remap(Manifest.Long))
    emitFieldDecl("loopSize", remap(Manifest.Long))

    emitMethod("alloc", actType, Nil) {
      emitValDef("__act", actType, createInstance(actType))
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
          case ParBuffer | ParSimpleBuffer =>
            stream.println("// " + fieldAccess("__act",quote(sym)) + " stays null for now")
          case ParFlat =>
            emitValDef(elem.buf.sV, typeCast("loopSize",remap(Manifest.Int)))
            allocGlobal(emitBlock(elem.buf.alloc))
            if (Config.generateSerializable) {
              val arraySym = if (!remap(elem.buf.alloc.tp).contains("DeliteArray")) fieldAccess(quote(getBlockResult(elem.buf.alloc)), dc_data_field(getBlockResult(elem.buf.alloc).tp)) else quote(getBlockResult(elem.buf.alloc))
              emitAssignment(fieldAccess(arraySym,"offset"), typeCast("loopStart",remap(Manifest.Int))) //FIXME: extremely hacky
            }
            emitAssignment(fieldAccess("__act",quote(sym)+"_data"),quote(getBlockResult(elem.buf.alloc)))
        }
        case (sym, elem: DeliteHashElem[_,_]) => //
        case (sym, elem: DeliteForeachElem[_]) =>
          emitAssignment(fieldAccess("__act",quote(sym)), remap(sym.tp), "()")  // must be type Unit, initialized in init below
        case (sym, elem: DeliteReduceElem[_]) =>
          emitBlock(elem.zero)
          emitAssignment(fieldAccess("__act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero)))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitBlock(elem.zero._1)
          emitAssignment(fieldAccess("__act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero._1)))
          emitBlock(elem.zero._2)
          emitAssignment(fieldAccess("__act",quote(sym)+"_zero_2"),quote(getBlockResult(elem.zero._2)))
      }
      emitReturn("__act")
    }


    emitMethod("main_par", actType, List(("__act", actType),("sync", syncType(actType)))) {
      emitProcessLocal(actType)
      if (!op.body.exists(b => loopBodyNeedsCombine(b) || loopBodyNeedsPostProcess(b))) emitBarrier()
      if (op.body.exists(loopBodyNeedsCombine)) {
        emitCombineLocal()
        emitCombineRemote()
        emitBarrier()
      }
      if (op.body.exists(loopBodyNeedsPostProcess)) {
        emitPostCombine(actType)
        emitBarrier()
        emitPostProcess()
        emitBarrier()
      }
      emitFinalizer()
      emitReturn("act")
    }

    emitMethod("main_seq", actType, List(("__act", actType))) {
      emitValDef("act", actType, methodCall("processRange", List("__act", "loopStart", "loopStart+loopSize")))
      if (op.body.exists(loopBodyNeedsPostProcess)) {
        emitMethodCall("postProcInit", List("act"))
        emitMethodCall("postProcess", List("act"))
      }
      emitMethodCall("finalize", List("act"))
      emitReturn("act")
    }

    //emit specialized input syms we want to manually loop hoist and be available in all methods
    //should be initialized manually in processRange()
    val inVars = getFreeVarBlock(Block(Combine(getMultiLoopFuncs(op,symList).map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct
    val extraArgs = inVars collect { sym => sym match {
      case s if s.tp == manifest[DeliteFileInputStream] => (quote(s)+"_stream", remap(s.tp))
    }}

    // processRange
    emitMethod("processRange", actType, List(("__act",actType),("start",remap(Manifest.Long)),("end",remap(Manifest.Long)))) {
      if (gcStrategy(op) == Global) emitHeapMark()
      //GROSS HACK ALERT: custom codegen for DeliteFileInputStream and DeliteFileOutputStream!
      val freeVars = getFreeVarBlock(Block(Combine(getMultiLoopFuncs(op,symList).map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct
      val inputStreamVars = freeVars.filter(_.tp == manifest[DeliteFileInputStream])
      val outputStreamVars = freeVars.filter(_.tp == manifest[DeliteFileOutputStream])

      if (inputStreamVars.length > 0) {
        assert(inputStreamVars.length == 1, "ERROR: don't know how to handle multiple input streams at once")
        val streamSym = quote(inputStreamVars(0))+"_stream"
        emitValDef(streamSym, remap(inputStreamVars(0).tp), fieldAccess(quote(inputStreamVars(0)),"openCopyAtNewLine(start)"))
        emitValDef(streamSym+"_offset", remap(manifest[Long]), fieldAccess(streamSym, "streamOffset"))
        emitValDef("isEmpty",remap(Manifest.Boolean), "end <= " + fieldAccess(streamSym,"position"))
        emitValDef("__act2",actType,methodCall("init",List("__act","-1","isEmpty",streamSym)))
        stream.println("while (" + fieldAccess(streamSym,"position") + " < " + streamSym + "_offset + end) {")
        if (gcStrategy(op) == Iteration) emitHeapMark()
        emitMethodCall("process",List("__act2","-1",streamSym))
        if (gcStrategy(op) == Iteration) emitHeapReset(List())
        stream.println("}")
        stream.println(fieldAccess(streamSym, "close();"))
      }
      else {
        emitValDef("isEmpty",remap(Manifest.Boolean),"end-start <= 0")
        emitVarDef("idx", remap(Manifest.Int), typeCast("start",remap(Manifest.Int)))
        emitValDef("__act2",actType,methodCall("init",List("__act","idx","isEmpty")))
        emitAssignment("idx","idx + 1")
        stream.println("while (idx < end) {")
        if (gcStrategy(op) == Iteration) emitHeapMark()
        emitMethodCall("process",List("__act2","idx"))
        if (gcStrategy(op) == Iteration) emitHeapReset(List())
        emitAssignment("idx","idx + 1")
        stream.println("}")
      }

      if (outputStreamVars.length > 0) {
        val streamSyms = outputStreamVars.map(s => quote(s))
        streamSyms foreach { s => emitUnalteredMethodCall(s + ".close", List(resourceInfoSym)) }
      }

      emitReturn("__act2")
    }

    // init and compute first element
    emitMethod("init", actType, List(("__act",actType),(quote(op.v),remap(op.v.tp)),("isEmpty",remap(Manifest.Boolean)))++extraArgs) {
      if (op.body exists (b => loopBodyNeedsCombine(b) || loopBodyNeedsPostProcess(b))) {
        emitValDef("__act2", actType, createInstance(actType))
        emitKernelMultiHashInit(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act2")
        (symList zip op.body) foreach {
          case (sym, elem: DeliteCollectElem[_,_,_]) => elem.par match {
            case ParBuffer | ParSimpleBuffer =>
              emitValDef(elem.buf.sV, "0")
              emitBlock(elem.buf.alloc)
              emitAssignment(fieldAccess("__act2",quote(sym)+"_buf"),quote(getBlockResult(elem.buf.alloc)))
            case ParFlat =>
              emitAssignment(fieldAccess("__act2",quote(sym)+"_data"),fieldAccess("__act",quote(sym)+"_data"))
            }
          case (sym, elem: DeliteHashElem[_,_]) =>
          case (sym, elem: DeliteForeachElem[_]) =>
            // this case only happens if a ForeachElem is fused with something else that needs combine
            emitVarDef(quote(sym), remap(sym.tp), "()")  //TODO: Need this for other targets? (Currently, other targets just don't generate unit types)
          case (sym, elem: DeliteReduceElem[_]) =>
            emitAssignment(fieldAccess("__act2",quote(sym)+"_zero"),fieldAccess("__act",quote(sym)+"_zero"))
            // should we throw an exception instead on an empty reduce?
            if (elem.stripFirst) {
              stream.println("if (isEmpty) // stripping the first iter: only initialize to zero if empty")
              emitAssignment(fieldAccess("__act2",quote(sym)),fieldAccess("__act2",quote(sym)+"_zero"))
            } else {
              if (isPrimitiveType(sym.tp)) {
                emitAssignment(fieldAccess("__act2",quote(sym)),fieldAccess("__act2",quote(sym)+"_zero"))
              } else {
                emitBlock(elem.accInit)
                emitAssignment(fieldAccess("__act2",quote(sym)),quote(getBlockResult(elem.accInit))) // separate zero buffer
              }
            }
          case (sym, elem: DeliteReduceTupleElem[_,_]) =>
            // no strip first here ... stream.println("assert(false, \"TODO: tuple reduce\")")
            emitAssignment(fieldAccess("__act2",quote(sym)+"_zero"), fieldAccess("__act",quote(sym)+"_zero"))
            emitAssignment(fieldAccess("__act2",quote(sym)+"_zero_2"), fieldAccess("__act",quote(sym)+"_zero_2"))
            emitAssignment(fieldAccess("__act2",quote(sym)), fieldAccess("__act2",quote(sym)+"_zero"))
            emitAssignment(fieldAccess("__act2",quote(sym)+"_2"), fieldAccess("__act2",quote(sym)+"_zero_2"))
        }
        // then emit first element initializers, if size is non-zero
        stream.println("if (!isEmpty) {")
        emitMultiLoopFuncs(op, symList)
        emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act2")
        (symList zip op.body) foreach {
          case (sym, elem: DeliteCollectElem[_,_,_]) =>
            emitCollectElem(op, sym, elem, "__act2")
          case (sym, elem: DeliteHashElem[_,_]) =>
          case (sym, elem: DeliteForeachElem[_]) =>
            emitForeachElem(op, sym, elem)
            emitAssignment(fieldAccess("__act2",quote(sym)),quote(sym))
          case (sym, elem: DeliteReduceElem[_]) =>
            if (elem.stripFirst) {
              emitFirstReduceElemAssign(op, sym, elem, "__act2")
            } else {
              emitReduceElem(op, sym, elem, "__act2")
            }
          case (sym, elem: DeliteReduceTupleElem[_,_]) =>
            emitReduceTupleElem(op, sym, elem, "__act2")
        }
        stream.println("}")
        emitReturn("__act2")
      } else {
        stream.println("if (!isEmpty) {")
        emitMethodCall("process", List("__act",quote(op.v)))
        stream.println("}")
        emitReturn("__act")
      }
    }

    emitMethod("process", remap(Manifest.Unit), List(("__act",actType),(quote(op.v),remap(op.v.tp)))++extraArgs) {
      emitMultiLoopFuncs(op, symList)
      emitMultiHashElem(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitCollectElem(op, sym, elem, "__act")
        case (sym, elem: DeliteHashElem[_,_]) => // done above
        case (sym, elem: DeliteForeachElem[_]) =>
          emitVarDef(quote(sym), remap(sym.tp), "()")
          emitForeachElem(op, sym, elem)
        case (sym, elem: DeliteReduceElem[_]) =>
          emitReduceElem(op, sym, elem, "__act")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitReduceTupleElem(op, sym, elem, "__act")
      }
    }

    emitMethod("combine", remap(Manifest.Unit), List(("__act",actType),("rhs",actType))) {
      emitMultiHashCombine(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (Config.generateSerializable) {
            val tpe = remap(sym.tp)
            val obj = if (tpe.contains("DeliteArrayObject")) tpe.take(tpe.indexOf("[")) else tpe
            emitAssignment(fieldAccess("__act",quote(sym)), obj+".combine(" + fieldAccess("__act",quote(sym)) + "," + fieldAccess("rhs",quote(sym)) + ")")
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) => // nothing needed
        case (sym, elem: DeliteReduceElem[_]) =>
          // if either value is zero, return the other instead of combining
          emitValDef(elem.rV._1, fieldAccess("__act",quote(sym)))
          emitValDef(elem.rV._2, fieldAccess("rhs", quote(sym)))
          stream.println("if (" + quote(elem.rV._1) + " == " + fieldAccess("__act", quote(sym) + "_zero") + ") {"/*}*/) //TODO: what if zero is an accumulator (SumIf)?
          emitAssignment(fieldAccess("__act",quote(sym)), quote(elem.rV._2))
          stream.println(/*{*/"}")
          stream.println("else if (" + quote(elem.rV._2) + " != " + fieldAccess("__act", quote(sym) + "_zero") + ") {"/*}*/) //TODO: see above
          emitBlock(elem.rFunc)
          emitAssignment(fieldAccess("__act",quote(sym)), quote(getBlockResult(elem.rFunc)))
          stream.println(/*{*/"}")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          // stream.println("assert(false, \"TODO: tuple reduce\")")
          val rV = elem.rVPar
          val rFunc = elem.rFuncPar
          emitValDef(rV._1._1, fieldAccess("__act",quote(sym)))
          emitValDef(rV._1._2, fieldAccess("__act",quote(sym)+"_2"))
          emitValDef(rV._2._1, fieldAccess("rhs",quote(sym)))
          emitValDef(rV._2._2, fieldAccess("rhs",quote(sym)+"_2"))
          emitFatBlock(List(rFunc._1, rFunc._2))
          emitAssignment(fieldAccess("__act",quote(sym)),quote(getBlockResult(rFunc._1)))
          emitAssignment(fieldAccess("__act",quote(sym)+"_2"), quote(getBlockResult(rFunc._2)))
      }
    }
    // scan/postprocess follows

    emitMethod("postCombine", remap(Manifest.Unit), List(("__act",actType),("lhs",actType))) {
      emitMultiHashPostCombine(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            emitAssignment(fieldAccess("__act", quote(sym) + "_offset"),fieldAccess("lhs", quote(sym) + "_offset") + "+" + fieldAccess("lhs", quote(sym) + "_size"))
            emitAssignment(fieldAccess("__act", quote(sym) + "_conditionals"),fieldAccess("__act", quote(sym) + "_conditionals") + "+" + fieldAccess("lhs", quote(sym) + "_conditionals"))
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
      //XX link act frames so we can set data later
      emitAssignment(fieldAccess("__act","left_act"), "lhs")
    }

    emitMethod("postProcInit", remap(Manifest.Unit), List(("__act",actType))) { // only called for last chunk!!
      emitMultiHashPostProcInit(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, actType, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            stream.println("if (" + fieldAccess("__act", quote(sym) + "_offset") + " > 0) {")
            emitValDef(elem.buf.sV, fieldAccess("__act", quote(sym) + "_offset") + " + " + fieldAccess("__act",quote(sym) + "_size"))
            emitValDef(elem.buf.allocVal, fieldAccess("__act", quote(sym) + "_buf"))
            allocGlobal(emitBlock(elem.buf.allocRaw))
            emitMethodCall(fieldAccess("__act",quote(sym) + "_data_set"),List(quote(getBlockResult(elem.buf.allocRaw)),fieldAccess("__act", quote(sym) + "_conditionals")))
            stream.println("} else {")
            emitMethodCall(fieldAccess("__act",quote(sym) + "_data_set"),List(fieldAccess("__act", quote(sym) + "_buf"),fieldAccess("__act", quote(sym) + "_conditionals")))
            stream.println("}")
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }

    emitMethod("postProcess", remap(Manifest.Unit), List(("__act",actType))) {
      emitMultiHashPostProcess(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, actType, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            // write size results from buf into data at offset
            stream.println("if (" + fieldAccess("__act",quote(sym)+"_data") + " " + refNotEq + " " + fieldAccess("__act",quote(sym)+"_buf") + ") {")
            emitValDef(elem.buf.sV, fieldAccess("__act",quote(sym)+"_size"))
            emitValDef(elem.buf.aV2, fieldAccess("__act",quote(sym)+"_buf"))
            emitValDef(elem.buf.allocVal, fieldAccess("__act",quote(sym)+"_data"))
            emitValDef(elem.buf.iV, "0")
            emitValDef(elem.buf.iV2, fieldAccess("__act",quote(sym)+"_offset"))
            emitBlock(elem.buf.copyRaw)
            stream.println("}")
            releaseRef(fieldAccess("__act",quote(sym)+"_buf"))
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }

    emitMethod("finalize", remap(Manifest.Unit), List(("__act",actType))) {
      emitMultiHashFinalize(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, "__act")
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitVarDef(quote(elem.buf.allocVal), remap(elem.buf.allocVal.tp), fieldAccess("__act",quote(sym) + "_data"))
          releaseRef(fieldAccess("__act",quote(sym)+"_data"))
          getActBuffer = List(quote(elem.buf.allocVal))
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            emitValDef(elem.buf.sV, fieldAccess("__act", quote(sym) + "_conditionals"))
            allocGlobal(emitBlock(elem.buf.setSize))
          }
          emitBlock(elem.buf.finalizer)
          emitAssignment(fieldAccess("__act",quote(sym)), quote(getBlockResult(elem.buf.finalizer)))
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
      if (gcStrategy(op) == Global) emitHeapReset(gcSyms(symList, op).map(quote))
    }

    //TODO: This would not be needed if other targets (CUDA, C, etc) properly creates activation records
    emitMethod("initAct", "activation_"+kernelName, List()) {
      emitValDef("act", "activation_"+kernelName, "new activation_"+kernelName)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
          emitBlock(elem.zero)
          emitAssignment(fieldAccess("act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero)))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitBlock(elem.zero._1)
          emitAssignment(fieldAccess("act",quote(sym)+"_zero"),quote(getBlockResult(elem.zero._1)))
          emitBlock(elem.zero._2)
          emitAssignment(fieldAccess("act",quote(sym)+"_zero_2"),quote(getBlockResult(elem.zero._2)))
        case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
          //emitAssignment(fieldAccess("act",quote(sym)+"_hash_data"), "new Array(128)")
        case (sym, elem: DeliteHashElem[_,_]) =>
      }

      val hashElems = (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }
      for ((cond,cps) <- hashElems.groupBy(_._2.cond)) {
        for((key,kps) <- cps.groupBy(_._2.keyFunc))
          emitAssignment(fieldAccess("act",kps.map(p=>quote(p._1)).mkString("")+"_hash_pos"), createInstance(hashmapType(remap(getBlockResult(key).tp)), List("512","128")))
      }
      emitReturn("act")
    }

    emitAbstractFatLoopFooter(symList, op) //end closure definition

    emitAssignment(fieldAccess(kernelName+"_closure", "loopStart"), "0")
    emitAssignment(fieldAccess(kernelName+"_closure", "loopSize"), quote(op.size))
    emitValDef("alloc", actType, methodCall(fieldAccess(kernelName+"_closure", "alloc"), List()))
    emitVarDef(kernelName, actType, nullRef)
    stream.println("if ("+fieldAccess(resourceInfoSym,"availableThreads")+" <= 1) {")
    emitAssignment(kernelName, methodCall(fieldAccess(kernelName+"_closure", "main_seq"), List("alloc")))
    stream.println("} else {")
    emitValDef("sync", syncType(actType), createInstance(syncType(actType), List(fieldAccess(kernelName+"_closure", "loopSize"), loopBodyAverageDynamicChunks(op.body).toString, resourceInfoSym)))
    emitVarDef("i", remap(Manifest.Int), "1")
    stream.println("while (i < "+unalteredMethodCall(fieldAccess("sync","numThreads"),List())+") {")
      emitValDef("r", resourceInfoType, unalteredMethodCall(fieldAccess("sync","getThreadResource"),List("i")))
      emitWorkLaunch(kernelName, "r", "alloc", "sync")
      emitAssignment("i", "i+1")
    stream.println("}")
    emitAssignment(kernelName, unalteredMethodCall(fieldAccess(kernelName+"_closure", "main_par"), List(unalteredMethodCall(fieldAccess("sync","getThreadResource"),List("0")),"alloc","sync")))
    stream.println("}")
  }

  def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    val kernelName = symList.map(quote).mkString("")
    val actType = "activation_" + kernelName
    emitClass(actType) {
      emitFieldDecl("left_act", actType)
      emitKernelMultiHashDecl(op, (symList zip op.body) collect { case (sym, elem: DeliteHashElem[_,_]) => (sym,elem) }, actType)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
          emitFieldDecl(quote(sym) + "_data", remap(elem.buf.allocVal.tp))
          if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
            emitFieldDecl(quote(sym) + "_buf", remap(elem.buf.allocVal.tp))
            emitFieldDecl(quote(sym) + "_size", remap(Manifest.Int))
            emitFieldDecl(quote(sym) + "_offset", remap(Manifest.Int))
            emitFieldDecl(quote(sym) + "_conditionals", remap(Manifest.Int))
            emitMethod(quote(sym)+"_data_set", remap(Manifest.Unit), List(("xs",remap(elem.buf.allocVal.tp)),("cs",remap(Manifest.Int)))) {
              emitAssignment(quote(sym) + "_data", "xs")
              emitAssignment(quote(sym) + "_conditionals", "cs")
              stream.println("if (left_act " + refNotEq + " " + nullRef + ")")
              emitMethodCall(fieldAccess("left_act",quote(sym)+"_data_set"),List("xs","cs")) // XX linked frame
            }
          }
        case (sym, elem: DeliteHashElem[_,_]) =>
        case (sym, elem: DeliteForeachElem[_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
        case (sym, elem: DeliteReduceElem[_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
          emitFieldDecl(quote(sym)+"_zero", remap(sym.tp))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitFieldDecl(quote(sym), remap(sym.tp))
          emitFieldDecl(quote(sym)+"_2", remap(elem.func._2.tp))
          emitFieldDecl(quote(sym)+"_zero", remap(sym.tp))
          emitFieldDecl(quote(sym)+"_zero_2", remap(elem.func._2.tp))
      }
      if (Config.generateSerializable) {
        emitUnalteredMethod("serialize", "java.util.ArrayList[com.google.protobuf.ByteString]", List()) {
          def serializeRef(sym: String) = "ppl.delite.runtime.messages.Serialization.serialize(this." + sym + ", true, \"" + sym + "\")"
          def serializeVal(sym: String, size: String = "-1") = "ppl.delite.runtime.messages.Serialization.serialize(this." + sym + ", 0, " + size + ")"

          emitValDef("arr", "java.util.ArrayList[com.google.protobuf.ByteString]", "new java.util.ArrayList")
          def prefix = "arr.add"
          var firstHash = true
          (symList zip op.body) foreach {
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              if (firstHash) {
                emitValDef("size", remap(Manifest.Int), kernelName+"_hash_pos.size")
                emitUnalteredMethodCall(prefix, List(serializeVal(kernelName+"_hash_pos.unsafeKeys", "size")))
                //emitMethodCall(prefix, List(serializeVal(kernelName+"_hash_pos.unsafeIndices"))) //TODO: remove this
                firstHash = false
              }
              emitUnalteredMethodCall(prefix, List(serializeVal(quote(sym)+"_hash_data", "size")))
            case (sym, elem: DeliteCollectElem[_,_,_]) =>
              emitUnalteredMethodCall(prefix, List(serializeRef(quote(sym))))
            case (sym, elem: DeliteForeachElem[_]) =>
            case (sym, elem: DeliteReduceElem[_]) =>
              emitUnalteredMethodCall(prefix, List(serializeVal(quote(sym))))
              //emitUnalteredMethodCall(prefix, List(serializeVal(quote(sym)+"_zero")))
            case (sym, elem: DeliteReduceTupleElem[_,_]) =>
              emitUnalteredMethodCall(prefix, List(serializeVal(quote(sym))))
              emitUnalteredMethodCall(prefix, List(serializeVal(quote(sym)+"_2")))
          }
          emitReturn("arr")
        }

        //TODO: This would not be needed if other targets (CUDA, C, etc) properly creates activation records
        //Target devices should send back the key array also, not just the data.
        //This unwrapping would only work for dense perfect hash cases.
        emitUnalteredMethod("unwrap", remap(Manifest.Unit), List()) {
          val keyGroups = (symList zip op.body) collect { case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => (sym,elem) } groupBy(_._2.keyFunc)
          for((key,kps) <- keyGroups) {
            val name = kps.map(p=>quote(p._1)).mkString("")
            emitVarDef("i_"+name, remap(Manifest.Int), "0")
            stream.println("while(i_"+name+" < " + fieldAccess(quote(kps(0)._1),"length") + ") {")
            emitUnalteredMethodCall(fieldAccess(name+"_hash_pos","put"),List("i_"+name))
            emitAssignment("i_"+name,"i_"+name+"+1")
            stream.println("}")
          }
          (symList zip op.body) foreach {
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              emitAssignment(quote(sym)+"_hash_data",quote(sym))
              releaseRef(quote(sym))
            case _ =>
          }
        }

      }
    }

    emitObject("activation_" + kernelName) {
      if (Config.generateSerializable) {
        emitUnalteredMethod("deserialize", "activation_"+kernelName, List(("bytes", "java.util.List[com.google.protobuf.ByteString]"))) {
          var idx = -1
          def deserialize(tp: String) = {
            idx += 1
            "ppl.delite.runtime.messages.Serialization.deserialize(classOf["+tp+"], bytes.get(" + idx + "))"
          }
          emitValDef("act", "activation_"+kernelName, "new activation_"+kernelName)
          val prefix = "act."
          var firstHash = true
          (symList zip op.body) foreach {
            case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
              if (firstHash) {
                val keyType = if (isPrimitiveType(elem.keyFunc.tp)) "ppl.delite.runtime.data.DeliteArray" + remap(elem.keyFunc.tp) else "ppl.delite.runtime.data.DeliteArrayObject[" + remap(elem.keyFunc.tp) + "]"
                emitValDef("keys", keyType, deserialize(keyType))
                //FIXME: kernelName is only correct if entire kernel is one hash_pos!
                emitAssignment(prefix+kernelName+"_hash_pos", "", "new generated.scala.container.HashMapImpl[" + remap(elem.keyFunc.tp) + "](512,128)")
                stream.println("for (i <- 0 until keys.length) " + prefix+kernelName+"_hash_pos.put(keys(i))") //FIXME!
                firstHash = false
              }
              emitAssignment(prefix+quote(sym)+"_hash_data", "", deserialize(remap(sym.tp)))
            case (sym, elem: DeliteCollectElem[_,_,_]) =>
              emitAssignment(prefix+quote(sym), "", deserialize(remap(sym.tp)))
            case (sym, elem: DeliteForeachElem[_]) =>
            case (sym, elem: DeliteReduceElem[_]) =>
              emitAssignment(prefix+quote(sym), "", deserialize(remap(sym.tp)))
              //emitAssignment(prefix+quote(sym)+"_zero", "", deserialize(remap(sym.tp)))
            case (sym, elem: DeliteReduceTupleElem[_,_]) =>
              emitAssignment(prefix+quote(sym), "", deserialize(remap(sym.tp)))
              emitAssignment(prefix+quote(sym)+"_2", "", deserialize(remap(elem.func._2.tp)))
          }
          emitReturn("act")
        }
      }
    }
  }

  override def emitFatNodeKernelExtra(sym: List[Sym[Any]], rhs: FatDef): Unit = rhs match {
    case op: AbstractFatLoop =>
      stream.println("//activation record for fat loop")
      emitAbstractFatLoopKernelExtra(op, sym)
    case _ =>
      super.emitFatNodeKernelExtra(sym, rhs)
  }

  override def emitNodeKernelExtra(sym: List[Sym[Any]], rhs: Def[Any]): Unit = rhs match {
    case op: AbstractLoop[_] =>
      stream.println("//activation record for thin loop")
      emitAbstractFatLoopKernelExtra(SimpleFatLoop(op.size, op.v, List(op.body)), sym)
    case _ =>
      super.emitNodeKernelExtra(sym, rhs)
  }

  override def traverseStm(stm: Stm) = {
    def kernelCall(lhs: List[Sym[Any]], rhs: Any) = {
      val kernelName = getKernelName(lhs)
      emitValDef("act_"+kernelName, getActType(kernelName), methodCall("kernel_"+kernelName, (inputVals(rhs)++inputVars(rhs)).map(quote)))
      for (s <- lhs) {
        emitValDef(s, fieldAccess("act_"+kernelName, quote(s)))
      }
    }

    if (Config.nestedParallelism) {
      stm match {
        case TP(lhs, rhs:AbstractLoop[_]) => 
          kernelCall(List(lhs), rhs)
          emitKernel(List(lhs), rhs)
        case TP(lhs, Reflect(rhs:AbstractLoop[_],_,_)) =>
          kernelCall(List(lhs), rhs)
          emitKernel(List(lhs), rhs)
        case TTP(lhs, mhs, rhs:AbstractFatLoop) => 
          kernelCall(lhs, rhs)
          emitKernel(lhs, rhs)
        case _ => super.traverseStm(stm)
      }
    } 
    else super.traverseStm(stm)  
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop => 
      loopLevel += 1
      if (!deliteKernel && !Config.nestedParallelism) emitInlineAbstractFatLoop(op, symList)
      else emitKernelAbstractFatLoop(op, symList)
      loopLevel -= 1
    case _ => super.emitFatNode(symList, rhs)
  }

}
