package ppl.delite.framework.ops

import java.io.{PrintWriter, StringWriter}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenerationFailedException


trait GPUGenDeliteOps extends GPUGenLoopsFat with BaseGenDeliteOps {
  import IR._

  var kernelName: String = _

  def emitVarDef(name: String, tpe: String, init: String) {
    tpe match {
      case "void" => //
      case _ =>
        stream.println(tpe + " " + name + " = " + init + ";")
    }
  }

  def emitValDef(name: String, tpe: String, init: String) {
    emitVarDef(name, tpe, init)
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop =>
      if (deliteKernel) emitKernelAbstractFatLoop(op, symList)
      else emitInlineAbstractFatLoop(op, symList)
    case _ => super.emitFatNode(symList, rhs)
  }

  def emitMultiLoopFuncs(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    val elemFuncs = op.body flatMap { // don't emit dependencies twice!
      //case elem: DeliteHashCollectElem[_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      //case elem: DeliteHashReduceElem[_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      //case elem: DeliteHashIndexElem[_,_] => elem.keyFunc :: elem.cond
      case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func)
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
      case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
    }
    // FIXME: without .distinct TPCHQ2 has duplicate definitions. this should be fixed in emitFatBlock.
    emitFatBlock(elemFuncs.distinct)
  }

  /*
  def emitFirstReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
      if (elem.cond.nonEmpty) {
        stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
        stream.println(quote(getBlockResult(elem.func)))
        stream.println("} else {")
        stream.println(prefixSym + quote(sym) + "_zero")
        stream.println("}")
      }
      else {
        stream.println(quote(getBlockResult(elem.func)))
      }
  }
  */

  def emitReduceElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
      emitReduction(op, sym, elem, prefixSym)
      stream.println("}")
    }
    else {
      emitReduction(op, sym, elem, prefixSym)
    }
  }

 def emitReduceTupleElem(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String = "") {
    if (elem.cond.nonEmpty){
      sys.error("tuple reduce with external conditions not implemented!")
    }
    else {
      emitReductionTuple(op, sym, elem, prefixSym)
    }
  }

  def emitReduction(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceElem[_], prefixSym: String = "") {
    stream.println("%s %s = %s;".format(remap(elem.rV._1.tp),quote(elem.rV._1),prefixSym+quote(sym)))
    stream.println("%s %s = %s;".format(remap(elem.rV._2.tp),quote(elem.rV._2),quote(getBlockResult(elem.func))))
    emitBlock(elem.rFunc)
    stream.println(prefixSym + quote(sym) + " = " + quote(getBlockResult(elem.rFunc)) + ";")
  }

  def emitReductionTuple(op: AbstractFatLoop, sym: Sym[Any], elem: DeliteReduceTupleElem[_,_], prefixSym: String) {
    val rV = elem.rVSeq
    val rFunc = elem.rFuncSeq

    stream.println("%s %s = %s;".format(remap(rV._1._1.tp),quote(rV._1._1),prefixSym+quote(sym)))
    stream.println("%s %s = %s_2;".format(remap(rV._1._2.tp),quote(rV._1._2),prefixSym+quote(sym)))
    stream.println("%s %s = %s;".format(remap(rV._2._1.tp),quote(rV._2._1),quote(getBlockResult(elem.func._1))))
    stream.println("%s %s = %s;".format(remap(rV._2._2.tp),quote(rV._2._2),quote(getBlockResult(elem.func._2))))
    emitFatBlock(List(rFunc._1, rFunc._2))
    stream.println(prefixSym + quote(sym) + "   = " + quote(getBlockResult(rFunc._1)) + ";")
    stream.println(prefixSym + quote(sym) + "_2 = " + quote(getBlockResult(rFunc._2)) + ";")
  }

  def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    (symList zip op.body) foreach {
      //TODO: Check if primitive type operations
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        boundMap.put(elem.buf.sV,op.size)
        emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quote(op.size))
        emitBlock(elem.buf.alloc) //This will generate alloc failure exception
        emitValDef(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
        emitVarDef(quote(sym) + "_size", remap(Manifest.Int), "0")
        //throw new GenerationFailedException("GPUGen: Inlined DeliteCollectElem is not supported yet due to memory allocations.\n" + quotePos(sym))
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
        emitBlock(elem.zero)
        stream.println("%s %s = %s;".format(remap(elem.zero.tp),quote(sym),quote(getBlockResult(elem.zero))))
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        emitFatBlock(List(elem.zero._1,elem.zero._2))
        stream.println("%s %s = %s;".format(remap(elem.zero._1.tp),quote(sym),quote(getBlockResult(elem.zero._1))))
        stream.println("%s %s_2 = %s;".format(remap(elem.zero._2.tp),quote(sym),quote(getBlockResult(elem.zero._2))))
      case _ =>
        throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
    }
    stream.println("int " + quote(op.v) + " = 0;")
    stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))

    // body
    emitMultiLoopFuncs(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
        emitValDef(elem.buf.allocVal, quote(sym)+"_data")
        emitBlock(elem.buf.update)
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println("//start emitReduceElem")
        emitReduceElem(op, sym, elem)
        stream.println("//end emitReduceElem")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println("//start emitReduceTupleElem")
        emitReduceTupleElem(op, sym, elem)
        stream.println("//end emitReduceTupleElem")
      case _ =>
        throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
    }
    stream.println(quote(op.v) + " += 1;")
    stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))

    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        emitValDef(elem.buf.allocVal, quote(sym) + "_data")
        emitBlock(elem.buf.finalizer)
        emitValDef(sym, quote(getBlockResult(elem.buf.finalizer)))
      case (sym, elem: DeliteForeachElem[_]) =>
      case (sym, elem: DeliteReduceElem[_]) =>
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
    }
  }

  def remapInputs(inputs: List[Sym[Any]], size: Exp[Int] = outerLoopSize, v: Sym[Int] = outerLoopSym.asInstanceOf[Sym[Int]]) : List[String] = {
      // last inputs always added to any device functions
      def lastInputs = (size match {
        case s@Sym(_) => List(v, size).map(i => remap(i.tp) + " " + quote(i))
        case _ => List(v).map(i => remap(i.tp) + " " + quote(i)) ++ List("int size")
      }) ++ List("TEMP_"+kernelName+" size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, activation_"+kernelName+" act")

      inputs.filter(_ != size).map(s =>
        if(inVars contains s)
          deviceTarget + "Ref" + remap(s.tp) + " " + quote(s)
        else
          remap(s.tp) + " " + quote(s)
      ) ++ lastInputs
  }

    def emitHashReduceElemProcess(op: AbstractFatLoop, symList: List[Sym[Any]]) {
      val keyGroups = (symList zip op.body) collect { case (sym, elem: DeliteHashReduceElem[_,_,_,_]) => (sym,elem) } groupBy (_._2.keyFunc)
      for (k <- keyGroups.keySet) {
        val funcs = op.body flatMap { case e: DeliteHashReduceElem[_,_,_,_] if (e.keyFunc==k) => e.keyFunc :: e.valFunc :: e.cond }
        val freeVars = getFreeVarBlock(Block(Combine(funcs.map(getBlockResultFull))),List(op.v)).filter(_ != op.size).distinct
        val inputs = ((symList zip op.body) collect { case (s, e: DeliteHashReduceElem[_,_,_,_]) if (e.keyFunc==k) => remap(e.mK)+" *"+quote(s)+"_key,"+remap(e.mV)+" *"+quote(s)+"_val" } ) ++ remapInputs(freeVars)

        val syms = keyGroups.get(k).get.map(_._1)
        for(sym <- syms) {
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          e.funcs += "key" -> syms.map(quote)
        }
        stream.println("__device__ void dev_process_" + funcNameSuffixSyms(syms) + "(" + inputs.mkString(",") + ") {")
        emitFatBlock(funcs.distinct)
        (symList zip op.body) foreach {
          case (s,e: DeliteHashReduceElem[_,_,_,_]) if (e.keyFunc==k) =>
            stream.println(quote(s) + "_key[" + quote(op.v) + "] = " + quote(getBlockResult(e.keyFunc)) + ";")
            stream.println(quote(s) + "_val[" + quote(op.v) + "] = " + quote(getBlockResult(e.valFunc)) + ";")
          case _ =>
        }
        stream.println("}")

      }
    }

  def funcNameSuffixSyms(syms: List[Sym[Any]]): String = {
    kernelName+"_"+syms.map(quote(_)).mkString("")
  }

  def funcNameSuffix(sym: Sym[Any]): String = funcNameSuffixSyms(List(sym))

    // emit process functions
  def emitProcessMethods(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    emitHashReduceElemProcess(op, symList)
    (symList zip op.body) foreach {
      case (sym, elem:DeliteCollectElem[_,_,_]) =>
        val freeVars = (getFreeVarBlock(Block(Combine((List(elem.func,elem.buf.update,elem.buf.appendable)++elem.cond).map(getBlockResultFull))),List(elem.buf.eV,elem.buf.allocVal,op.v,sym))++List(sym)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        //emitBlock(elem.func)
        //emitValDef(elem.eV, quote(getBlockResult(elem.func)))
        elem.par match {
          case ParSimpleBuffer =>
            //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
            emitFatBlock(elem.cond)
            if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
            emitBlock(elem.buf.appendable)
            stream.println("if (" + quote(getBlockResult(elem.buf.appendable)) + ") {")
            //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
            //emitBlock(elem.update)
            stream.println("act." + quote(sym) + "_bitmap[" + quote(op.v) + "] = 1;")
            stream.println("}")
            if (elem.cond.nonEmpty) {
              // Need this for GPU?
              // stream.println(quote(sym) + "_conditionals[" + quote(op.v) + "] += 1;")
              stream.println("}")
            }
          case ParFlat =>
            emitBlock(elem.func)
            emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
            emitValDef(elem.buf.allocVal, quote(sym))
            emitBlock(elem.buf.update)
          case _ =>
        }
        stream.println("}")

      case (sym, elem:DeliteForeachElem[_]) =>
        val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.func)
        stream.println("}")

      case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.func)
        stream.println("return " + quote(getBlockResult(elem.func)) + ";")
        stream.println("}")

      case (sym, elem:DeliteReduceElem[_]) =>
        val freeVars = getFreeVarBlock(Block(Combine((List(elem.func,elem.rFunc,elem.zero)++elem.cond).map(getBlockResultFull))),List(elem.rV._1,elem.rV._2,op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.rV._1))
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.func)
        emitValDef(elem.rV._2, quote(getBlockResult(elem.func)))
        if(elem.cond.nonEmpty) {
          emitFatBlock(elem.cond)
          stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
          if (elem.stripFirst) {
            emitBlock(elem.zero)
            stream.println(remap(sym.tp) + " " + quote(sym) + "_zero = " + quote(getBlockResult(elem.zero)) + ";")
            stream.println("if(" + quote(elem.rV._1) + " == " + quote(sym) + "_zero) {")
            stream.println("return " + quote(elem.rV._2) + ";")
            stream.println("}")
            stream.println("else {")
            emitBlock(elem.rFunc)
            stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
            stream.println("}")
          }
          else {
            emitBlock(elem.rFunc)
            stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
          }
          stream.println("}")
          stream.println("else {")
          stream.println("return " + quote(elem.rV._1) + ";")
          stream.println("}")
        }
        else {
          emitBlock(elem.rFunc)
          stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
        }
        stream.println("}")

      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        val freeVars = getFreeVarBlock(Block(Combine((List(elem.func._1,elem.func._2,elem.rFuncSeq._1,elem.rFuncSeq._2)++elem.cond).map(getBlockResultFull))),List(elem.rVSeq._1._1,elem.rVSeq._1._2,elem.rVSeq._2._1,elem.rVSeq._2._2,op.v)).filter(_ != op.size).distinct
        val e = metaData.outputs.get(sym).get
        e.funcs += "process" -> freeVars.map(quote)
        for(i <- 1 until 3) {
          val (rVSeq1, rVSeq2, rFuncSeq) = if(i == 1) (elem.rVSeq._1._1, elem.rVSeq._2._1, elem.rFuncSeq._1)
                                           else (elem.rVSeq._1._2, elem.rVSeq._2._2, elem.rFuncSeq._2)
          val inputs = remapInputs(freeVars ++ List(elem.rVSeq._1._1,elem.rVSeq._1._2))
          stream.println("__device__ " + remap(sym.tp) + " dev_process" + i + "_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitFatBlock(List(elem.func._1,elem.func._2))
          emitValDef(elem.rVSeq._2._1, quote(getBlockResult(elem.func._1)))
          emitValDef(elem.rVSeq._2._2, quote(getBlockResult(elem.func._2)))
          if(elem.cond.nonEmpty) {
            emitFatBlock(elem.cond)
            stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
            assert(!elem.stripFirst)
            emitBlock(rFuncSeq)
            stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
            stream.println("}")
            stream.println("else {")
            stream.println("return " + quote(rVSeq1) + ";")
            stream.println("}")
          }
          else {
            emitBlock(rFuncSeq)
            stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
          }
          stream.println("}")
        }

      case _ => //
    }
  }

  def emitKernelAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    outerLoopSize = op.size
    outerLoopSym = op.v
    tabWidth += 1
    kernelName = symList.map(quote).mkString("")

    // register metadata for each elem and check GenerationFailedException conditions
    (symList zip op.body) foreach { s =>
      s match {
        case (sym, elem:DeliteCollectElem[_,_,_]) =>
          if (elem.par == ParBuffer) throw new GenerationFailedException("GPUGen DeliteOps: ParBuffer is not supported.")
          if (!isPrimitiveType(elem.mA)) throw new GenerationFailedException("GPUGen DeliteOps: output of collect elem is non-primitive type.")
          if (elem.iFunc.isDefined) throw new GenerationFailedException("GPUGen DeliteOps: Flatmap is now allowed for GPU")
          if(elem.par == ParFlat)
            metaData.outputs.put(sym, new LoopElem("COLLECT",Map("mA"->remap(elem.mA),"mI"->remap(elem.mI),"mCA"->remap(elem.mCA))))
          else
            metaData.outputs.put(sym, new LoopElem("COLLECT_BUF",Map("mA"->remap(elem.mA),"mI"->remap(elem.mI),"mCA"->remap(elem.mCA))))
        case (sym, elem:DeliteForeachElem[_]) =>
          metaData.outputs.put(sym, new LoopElem("FOREACH",Map("mA"->remap(elem.mA))))
          /*throw new GenerationFailedException("GPUGen DeliteOps: GPU ForEachElem is temporarily disabled..")
          metaData.outputs.put(sym,new TransferFunc)
          lf.tpe = "FOREACH"*/
        case (sym, elem: DeliteReduceElem[_]) =>
          if(!isPrimitiveType(sym.tp)) {
            if(encounteredZipWith contains getBlockResult(elem.rFunc)) {
              val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
              if(isPrimitiveType(z.dmR)) metaData.outputs.put(sym, new LoopElem("REDUCE_SPEC",Map("mA"->remap(elem.mA),"dmR"->remap(z.dmR))))
              else throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceElem with non-primitive types is not supported.")
            }
            else {
              throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceElem with non-primitive types is not supported.")
            }
          }
          else {
            metaData.outputs.put(sym, new LoopElem("REDUCE",Map("mA"->remap(elem.mA))))
          }
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          if(!isPrimitiveType(sym.tp)) throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceTupleElem with non-primitive types is not supported.")
          if(elem.cond.nonEmpty && elem.stripFirst) throw new GenerationFailedException("GPUGen DeliteOps: DeliteReduceTupleElem with condition + stripFirst is not supported.")
          metaData.outputs.put(sym, new LoopElem("REDUCE_TUPLE",Map("mA"->remap(elem.mA),"mB"->remap(elem.mB))))
        case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>

          // Currently only support limited types of hash-reduce on GPU
          // keys should be dense perfect hash (0 ~ N-1 for N keys)
          // reduction needs to be primitive type reduction
          //if(elem.cond.nonEmpty) throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem with condition is not supported.")
          //if(!isPrimitiveType(elem.mV)) throw new GenerationFailedException("GPUGen DeliteOPs: DeliteHashReduceElem only supports primitve type reduction.")
          if(remap(elem.mK) != "int") throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem only supports perfect hash.")

          if(!isPrimitiveType(elem.mV)) {
            if(encounteredZipWith contains getBlockResult(elem.rFunc)) {
              val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
              if(isPrimitiveType(z.dmR)) metaData.outputs.put(sym, new LoopElem("HASH_REDUCE_SPEC",Map("mK"->remap(elem.mK),"mV"->remap(elem.mV),"mCV"->remap(elem.mCV),"dmR"->remap(z.dmR))))
              else throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem with non-primitive types is not supported.")
            }
            else {
              throw new GenerationFailedException("GPUGen DeliteOps: DeliteHashReduceElem with non-primitive types is not supported.")
            }
          }
          else {
            metaData.outputs.put(sym, new LoopElem("HASH_REDUCE",Map("mK"->remap(elem.mK),"mV"->remap(elem.mV),"mCV"->remap(elem.mCV))))
          }
        case (sym, _) =>
          throw new GenerationFailedException("GPUGen DeliteOps: Unsupported Elem type for " + quote(sym))
      }
    }

    isNestedNode = true;

    // emit init functions
    (symList zip op.body) foreach {
      case (sym, elem:DeliteReduceElem[_]) =>
        val initFunc = if(elem.stripFirst || isPrimitiveType(sym.tp)) elem.zero else elem.accInit
        val freeVars = getFreeVarBlock(initFunc,Nil).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "init" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_init_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(initFunc)
        stream.println("return " + quote(getBlockResult(initFunc)) + ";")
        stream.println("}")
        if(e.elemType == "REDUCE_SPEC") {
          val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
          stream.println("__device__ " + remap(z.dmR) + " dev_spcinit_" + funcNameSuffix(sym) + "(void) { return 0; }")
        }
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        //TODO: would it affect the performance to have separate inputs for zero1 and zero2?
        val freeVars = getFreeVarBlock(Block(Combine(List(elem.zero._1,elem.zero._2).map(getBlockResultFull))),Nil).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "init" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_init1_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero._1)
        stream.println("return " + quote(getBlockResult(elem.zero._1)) + ";")
        stream.println("}")
        stream.println("__device__ " + remap(sym.tp) + " dev_init2_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero._2)
        stream.println("return " + quote(getBlockResult(elem.zero._2)) + ";")
        stream.println("}")
      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        val freeVars = getFreeVarBlock(elem.zero,Nil).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "init" -> freeVars.map(quote)
        stream.println("__device__ " + remap(elem.mV) + " dev_init_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero)
        stream.println("return " + quote(getBlockResult(elem.zero)) + ";")
        stream.println("}")
        if(e.elemType == "HASH_REDUCE_SPEC") {
          val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
          stream.println("__device__ " + remap(z.dmR) + " dev_spcinit_" + funcNameSuffix(sym) + "(void) { return 0; }")
        }
      case _ => //
    }

    emitProcessMethods(op, symList)

    // emit post-process functions
    (symList zip op.body) foreach {
      case (sym, elem:DeliteCollectElem[_,_,_]) =>
        val freeVars = (getFreeVarBlock(Block(Combine((List(elem.func,elem.buf.update)++elem.cond).map(getBlockResultFull))),List(elem.buf.eV,elem.buf.allocVal,op.v,sym))++List(sym)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars)
        val e = metaData.outputs.get(sym).get
        e.funcs += "postprocess" -> freeVars.map(quote)
        stream.println("__device__ void dev_postprocess_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        elem.par match {
          case ParSimpleBuffer =>
            emitValDef(elem.buf.allocVal, quote(sym))
            if (elem.cond.nonEmpty) stream.println("if (act."+quote(sym)+"_bitmap[" + quote(op.v) + "] == 1) {")
            emitBlock(elem.func)
            emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
            stream.println(quote(op.v) + " = act." + quote(sym) + "_scanmap[" + quote(op.v) + "];")
            emitBlock(elem.buf.update)
            if (elem.cond.nonEmpty) stream.println("}")
          case _ =>
        }
        stream.println("}")

      case _ =>
    }

    // emit combine functions
    (symList zip op.body) foreach {
      case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        /*
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val freeVars = getFreeVarBlock(Block(Combine(List(zbody.func).map(getBlockResultFull))),List(z.inA.asInstanceOf[Sym[_]],z.inB.asInstanceOf[Sym[_]],z.v)).distinct
        val inputs = remapInputs(freeVars ++ List(z.inA.asInstanceOf[Sym[_]],z.inB.asInstanceOf[Sym[_]]),z.size,z.v) //Hack : Set size to be a const!
        val e = metaData.outputs.getOrElse(sym,new LoopElem)
        e.loopReduceInputs = freeVars.map(quote)
        stream.println("__device__ " + remap(z.dmR) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(zbody.func)
        stream.println("return " + quote(getBlockResult(zbody.func)) + ";")
        stream.println("}")
        */
        // FIXIT: Hacky way of generating zip function
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val prevInnerScope = innerScope
        val result = zbody.func
        result.res match {
          case r:Sym[_] if(innerScope==null) => innerScope = List(findDefinition(r).get)
          case r:Sym[_] => innerScope = findDefinition(r).get :: innerScope
          case _ => //
        }
        val freeVars = getFreeVarBlock(Block(Combine(List(zbody.func).map(getBlockResultFull))),List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]])).filter(_ != op.size).distinct
        //val inputs = (freeVars ++ List(z.fin._1,z.fin._2)).map(i => remap(i.tp) + " " + quote(i))
        val inputs = remapInputs(freeVars ++ List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]]))
        stream.println("__device__ " + remap(z.dmR) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(result)
        stream.println("return " + quote(getBlockResult(result)) + ";")
        stream.println("}")
        innerScope = prevInnerScope
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)

      case (sym, elem:DeliteReduceElem[_]) =>
        val freeVars = getFreeVarBlock(Block(Combine(List(elem.rFunc,elem.zero).map(getBlockResultFull))),List(elem.rV._1,elem.rV._2,op.v)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.rV._1,elem.rV._2))
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        stream.println("__device__ " + remap(sym.tp) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.zero)
        stream.println(remap(sym.tp) + " " + quote(sym) + "_zero = " + quote(getBlockResult(elem.zero)) + ";")
        if(elem.cond.nonEmpty) {
          stream.println("if (" + quote(elem.rV._1) + " == " + quote(sym) + "_zero) {")
          stream.println("return " + quote(elem.rV._2) + ";")
          stream.println("}")
          stream.println("else if(" + quote(elem.rV._2) + " != " + quote(sym) + "_zero) {")
          emitBlock(elem.rFunc)
          stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
          stream.println("}")
          stream.println("else {")
          stream.println("return " + quote(elem.rV._1) + ";")
          stream.println("}")
        }
        else {
          emitBlock(elem.rFunc)
          stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
        }
        stream.println("}")

      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        val freeVars = getFreeVarBlock(Block(Combine(List(elem.rFuncPar._1,elem.rFuncPar._2).map(getBlockResultFull))),List(elem.rVPar._1._1,elem.rVPar._1._2,elem.rVPar._2._1,elem.rVPar._2._2,op.v)).filter(_ != op.size).distinct
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        for(i <- 1 until 3) {
          val (func, rFuncPar) = if(i == 1) (elem.func._1, elem.rFuncPar._1)
                                 else (elem.func._2, elem.rFuncPar._2)
          val inputs = remapInputs(freeVars ++ List(elem.rVPar._1._1,elem.rVPar._1._2,elem.rVPar._2._1,elem.rVPar._2._2))
          stream.println("__device__ " + remap(sym.tp) + " dev_combine" + i + "_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitBlock(rFuncPar)
          stream.println("return " + quote(getBlockResult(rFuncPar)) + ";")
          stream.println("}")
        }

      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        // FIXIT: Hacky way of generating zip function
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val prevInnerScope = innerScope
        val result = zbody.func
        result.res match {
          case r:Sym[_] if(innerScope==null) => innerScope = List(findDefinition(r).get)
          case r:Sym[_] => innerScope = findDefinition(r).get :: innerScope
          case _ => //
        }
        val freeVars = getFreeVarBlock(Block(Combine(List(zbody.func).map(getBlockResultFull))),List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]])).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(z.fin._1.asInstanceOf[Sym[_]],z.fin._2.asInstanceOf[Sym[_]]))
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        stream.println("__device__ " + remap(z.dmR) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(result)
        stream.println("return " + quote(getBlockResult(result)) + ";")
        stream.println("}")
        innerScope = prevInnerScope

      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        val freeVars = getFreeVarBlock(elem.rFunc,List(elem.rV._1,elem.rV._2)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.rV._1,elem.rV._2))
        val e = metaData.outputs.get(sym).get
        e.funcs += "combine" -> freeVars.map(quote)
        stream.println("__device__ " + remap(elem.mV) + " dev_combine_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
        emitBlock(elem.rFunc)
        stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
        stream.println("}")


      case _ => //
    }

    isNestedNode = false;

    // emit output allocations
    val actName = symList.map(quote).mkString("")
    (symList zip op.body) foreach {
      case (sym, elem:DeliteCollectElem[_,_,_]) =>
        val e = metaData.outputs.get(sym).get
        val allocInputs = elem.par match {
          case ParSimpleBuffer =>
            //TODO: enable below by generating both alloc and allocRaw
            if (getFreeVarBlock(elem.buf.allocRaw,List(elem.buf.sV)).isEmpty)
              emitMultiLoopAllocFunc(elem.buf.allocRaw,"alloc_"+quote(sym),actName,quote(sym)+"_data",Map(elem.buf.sV->("act."+quote(sym)+"_conditionals")))
            else
              throw new GenerationFailedException("allocRaw block refers to alloc block")
          case _ => emitMultiLoopAllocFunc(elem.buf.alloc,"alloc_"+quote(sym),actName,quote(sym)+"_data",Map(elem.buf.sV->("act.size")))
        }
        e.funcs += "alloc" -> allocInputs.map(quote)
        val finalizerInputs = emitMultiLoopAllocFunc(elem.buf.finalizer,"finalizer_"+quote(sym),actName,quote(sym),Map(elem.buf.allocVal->("act."+quote(sym)+"_data")))
        e.funcs += "finalizer" -> finalizerInputs.map(quote)

      case (sym, elem:DeliteForeachElem[_]) =>
        val e = metaData.outputs.get(sym).get

      //TODO: Fix below alloc func to use a correct one.
      case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
        val e = metaData.outputs.get(sym).get
        val z = encounteredZipWith.get(getBlockResult(elem.rFunc)).get
        val zbody = z.body.asInstanceOf[DeliteCollectElem[_,_,_]]
        val allocInputs = emitMultiLoopAllocFunc(elem.zero, "alloc_"+quote(sym), actName, quote(sym), Map())
        e.funcs += "alloc" -> allocInputs.map(quote)

      case (sym, elem: DeliteReduceElem[_]) =>
        val e = metaData.outputs.get(sym).get
        val allocInputs = emitMultiLoopAllocFunc(elem.zero, "alloc_"+quote(sym), actName, quote(sym), Map())
        e.funcs += "alloc" -> allocInputs.map(quote)

      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        val e = metaData.outputs.get(sym).get
        assert(isPrimitiveType(sym.tp))
        val allocInputs = emitMultiLoopAllocFunc(elem.zero._1, "alloc_"+quote(sym), actName, quote(sym), Map())
        e.funcs += "alloc" -> allocInputs.map(quote)

      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        val e = metaData.outputs.get(sym).get
        val allocInputs = emitMultiLoopAllocFunc(elem.buf.alloc, "alloc_"+quote(sym), actName, quote(sym), Map(elem.buf.sV->("act."+quote(sym)+"_numKeys")))
        e.funcs += "alloc" -> allocInputs.map(quote)

        // Generate update function
        val freeVars = getFreeVarBlock(elem.buf.update,List(elem.buf.allocVal,elem.buf.iV,elem.buf.eV)).filter(_ != op.size).distinct
        val inputs = remapInputs(freeVars ++ List(elem.buf.allocVal,elem.buf.iV,elem.buf.eV))
        e.funcs += "update" -> freeVars.map(quote)
        stream.println("__device__ void " + quote(sym) + "_update(" + inputs.mkString(",") + ") {")
        emitBlock(elem.buf.update)
        stream.println("}")

      case _ =>
    }

    tabWidth -= 1
  }

  // Emit Activation Record
  def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    val stream = actRecordStream
    val kernelName = symList.map(quote).mkString("")
    stream.println("#ifndef __ACT_" + kernelName + "__")
    stream.println("#define __ACT_" + kernelName + "__")
    stream.println("typedef struct {")
    stream.println("unsigned int size;")
    (symList zip op.body) foreach {
      case (sym, elem: DeliteCollectElem[_,_,_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
        stream.println(remap(elem.buf.allocVal.tp) + " *" + quote(sym) + "_data;")
        if (elem.par == ParBuffer || elem.par == ParSimpleBuffer) {
          //stream.println(remap(elem.eV) + " *" + quote(sym) + "_buf;")
          stream.println("unsigned int *" + quote(sym) + "_bitmap;")
          stream.println("unsigned int *" + quote(sym) + "_scanmap;")
          stream.println("unsigned int " + quote(sym) + "_conditionals;")
        }
      case (sym, elem: DeliteReduceElem[_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
      case (sym, elem: DeliteReduceTupleElem[_,_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
      case (sym, elem: DeliteHashReduceElem[_,_,_,_]) =>
        stream.println(remap(sym.tp) + " *" + quote(sym) + ";")
        stream.println("unsigned int " + quote(sym) + "_numKeys;")
      case _ => //
    }
    stream.println("} activation_" + kernelName + ";")
    stream.println("#endif")
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      val b = s.block
      if (!deliteKernel) {  //In the process of generating operations for deliteKernel type kernels (allow SingleTask to be inlined)
        emitBlock(b)
        if(!isVoidType(sym.tp)) {
          emitValDef(sym, quote(getBlockResult(b)))
          emitPtrDef(sym, getBlockResult(b))
        }
      }
      else {
        throw new GenerationFailedException("GPUGen: DeliteOpSingleTask is not GPUable")
      }
    }

    case op: AbstractLoop[_] =>
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))

    case _ => super.emitNode(sym,rhs)
  }

  def emitMultiLoopAllocFunc(block: Block[Any], funcName: String, actType: String, resultActField: String, boundVals:Map[Sym[Any],String]): List[Sym[Any]] = {
    processingHelperFunc = true

    val out = new StringBuilder
    val blockString = new StringWriter
    val blockStream = new PrintWriter(blockString,true)

    val inputs = getFreeVarBlock(block,boundVals.keySet.toList)

    if(isPrimitiveType(block.tp)) { // primitive type allocation
      if(actType == "") throw new GenerationFailedException("Non-DeliteOp with primitive type output.")
      blockString.append("\tDeliteCudaMalloc((void**)&(act." + resultActField + "), sizeof(%s));\n".format(remap(block.tp)))
    }
    else {
      // emit bouding symbols
      for (b <- boundVals) {
        withStream(blockStream) {
          if(isPrimitiveType(b._1.tp)) emitValDef(b._1,b._2)
          else {
            stream.println("\t%s *%s_ptr = %s;".format(remap(b._1.tp),quote(b._1),b._2))
            stream.println("\t%s %s = *(%s_ptr);".format(remap(b._1.tp),quote(b._1),quote(b._1)))
          }
        }
      }

      // emit block body
      withStream(blockStream) {
        emitBlock(block)
        if(actType!="") stream.println("\tact." + resultActField + " = " + quote(getBlockResult(block)) + "_ptr;")
        else stream.println("\t*" + resultActField + " = " + quote(getBlockResult(block)) + "_ptr;")
      }
    }

    val act = if(actType=="") remap(block.tp) + " **" + resultActField else "activation_" + actType + " &act"
    val paramStr = (inputs.map(s =>
      if(isPrimitiveType(s.tp)) remap(s.tp) + " " + quote(s)
      else remap(s.tp) + " *" + quote(s) + "_ptr"
    ) :+ act).mkString(",")
    val derefParams = inputs.map(s =>
      if(isPrimitiveType(s.tp)) ""
      else "\t%s %s = *(%s_ptr);\n".format(remap(s.tp),quote(s),quote(s))
    ).mkString("")

    // emit header and complete host function
    out.append("void %s(%s)".format(funcName, paramStr))
    headerStream.append(out.toString + ";\n")
    out.append("{\n")
    out.append(derefParams)
    out.append(blockString)
    out.append("}\n")
    if(!helperFuncList.contains(funcName+actType)) helperFuncStream.println(out.toString)
    helperFuncList += funcName+actType

    processingHelperFunc = false
    inputs
  }

  def emitMultiLoopFunc(func:Block[Any], postfix: String, lastInputs: List[Sym[Any]], stream:PrintWriter): List[String] = {
    isNestedNode = true
    val tempString = new StringWriter
    val tempStream = new PrintWriter(tempString, true)
    val header = new StringWriter
    val footer = new StringWriter

    val currentTab = tabWidth
    tabWidth = 1
    withStream(tempStream) {
      emitBlock(func)
    }
    tabWidth = currentTab

    def wrapRef(sym: Exp[Any]):String = {
      if(inVars contains sym)
        deviceTarget + "Ref" + remap(sym.tp) + addRef(sym.tp)
      else
        remap(sym.tp)
    }

    val inputs = getFreeVarBlock(func,lastInputs).distinct
    val paramStr = ((inputs++lastInputs).map(ele => wrapRef(ele) + " " + quote(ele)) ++ metaData.temps.map(t=>t.tp + " *" + t.sym) ++ List("size_t tempMemSize","char *tempMemPtr","int *tempMemUsage")).mkString(",")

    header.append(devFuncPrefix + " %s dev_%s(%s) {\n".format(remap(getBlockResult(func).tp),postfix,paramStr))
    if(remap(getBlockResult(func).tp) != "void")
      footer.append("\treturn %s;\n".format(quote(getBlockResult(func))))
    footer.append("}\n")
    stream.print(header)
    stream.print(tempString)
    stream.print(footer)

    isNestedNode = false
    inputs.map(quote(_))
  }

}

trait GPUGenDeliteOpsOpt extends GPUGenDeliteOps {
  import IR._

  var multiDimMapping: Boolean = false
  var currentLoopLevel: Int = 0
  var maxLoopLevel: Int = 0

  private def getDimIdx(level: Int): String = {
    loopAnalysisResult.get(level) match {
      case Some((DimX,_,_)) => "threadIdx.x"
      case Some((DimY,_,_)) => "threadIdx.y"
      case Some((DimZ,_,_)) => "threadIdx.z"
      case _ => throw new GenerationFailedException("getCurrentDimIdx")
    }
  }

  private def getDimGlobalIdx(level: Int): String = {
    loopAnalysisResult.get(level) match {
      case Some((DimX,_,_)) => "blockIdx.x * blockDim.x + threadIdx.x"
      case Some((DimY,_,_)) => "blockIdx.y * blockDim.y + threadIdx.y"
      case Some((DimZ,_,_)) => "blockIdx.z * blockDim.z + threadIdx.z"
      case _ => throw new GenerationFailedException("getCurrentDimGlobalIdx")
    }
  }

  private def getDimStride(level: Int): String = {
    loopAnalysisResult.get(level) match {
      case Some((DimX,_,_)) => "gridDim.x * blockDim.x"
      case Some((DimY,_,_)) => "gridDim.y * blockDim.y"
      case Some((DimZ,_,_)) => "gridDim.z * blockDim.z"
      case _ => throw new GenerationFailedException("getCurrentDimGlobalIdx")
    }
  }

  private def getCurrentDimIdx: String = getDimIdx(currentLoopLevel)
  private def getCurrentDimGlobalIdx: String = getDimGlobalIdx(currentLoopLevel)
  private def getCurrentDimStride: String = getDimStride(currentLoopLevel)

  private def getDimSize(level: Int): String = {
    loopAnalysisResult.get(level) match {
    // Note: using blockDim.x to allocate shared memory is not allowed.
    // They should be constants
/*
      case Some((DimX,_,_)) => "blockDim.x"
      case Some((DimY,_,_)) => "blockDim.y"
      case Some((DimZ,_,_)) => "blockDim.z"
*/
      case Some((_,size,_)) => size.toString
      case _ => throw new GenerationFailedException("getCurrentDimSize")
    }
  }

  private def getCurrentDimSize: String = getDimSize(currentLoopLevel)

  private def getOuterLoopDimSizes: List[String] = {
    val indices = (0 until loopAnalysisResult.size).filter(_ < currentLoopLevel)
    indices.map(getDimSize).toList
  }

  private def getInnerLoopDimIndices: List[String] = {
    val indices = (0 until loopAnalysisResult.size).filter(_ > currentLoopLevel)
    indices.map(getDimIdx).toList
  }
  private def getOuterLoopDimIndices: List[String] = {
    val indices = (0 until loopAnalysisResult.size).filter(_ < currentLoopLevel)
    indices.map(getDimIdx).toList
  }
  def getInnerLoopGuard: String = {
    val indices = (0 until loopAnalysisResult.size).filter(_ > currentLoopLevel)
    "if(" + indices.map(i => "(" + getDimIdx(i) + "== 0)").mkString("&&") + ")"
  }


  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case op: AbstractFatLoop if (loopAnalysisResult.size > 1) =>
      multiDimMapping = true
      maxLoopLevel = loopAnalysisResult.size - 1
      if (deliteKernel) {
        currentLoopLevel = 0
        emitKernelAbstractFatLoop(op, symList)
        metaData.auxMeta.append(("multiDim","[" + loopAnalysisResult.map(m => "{\"level\":\"" + m._1 + "\"," + m._2._1 + ",\"size\":\"" + m._2._2 + "\"," + m._2._3.toString(quote) + "}").mkString(",") + "]"))
      }
      else { currentLoopLevel += 1; emitInlineAbstractFatLoop(op, symList); currentLoopLevel -= 1; }
    case _ =>
      multiDimMapping = false
      super.emitFatNode(symList, rhs)
  }

  override def emitInlineAbstractFatLoop(op: AbstractFatLoop, symList: List[Sym[Any]]) {
    if (multiDimMapping == false) {
      super.emitInlineAbstractFatLoop(op, symList)
    }
    else {
      (symList zip op.body) foreach {
        //TODO: Check if primitive type operations
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          boundMap.put(elem.buf.sV,op.size)
          emitVarDef(quote(elem.buf.sV), remap(elem.buf.sV.tp), quote(op.size))
          emitBlock(elem.buf.alloc) //This will generate alloc failure exception
          emitValDef(quote(sym) + "_data", remap(getBlockResult(elem.buf.alloc).tp), quote(getBlockResult(elem.buf.alloc)))
          emitVarDef(quote(sym) + "_size", remap(Manifest.Int), "0")
          //throw new GenerationFailedException("GPUGen: Inlined DeliteCollectElem is not supported yet due to memory allocations.\n" + quotePos(sym))
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
          emitBlock(elem.zero)
          stream.println("%s %s = %s;".format(remap(elem.zero.tp),quote(sym),quote(getBlockResult(elem.zero))))
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          emitFatBlock(List(elem.zero._1,elem.zero._2))
          stream.println("%s %s = %s;".format(remap(elem.zero._1.tp),quote(sym),quote(getBlockResult(elem.zero._1))))
          stream.println("%s %s_2 = %s;".format(remap(elem.zero._2.tp),quote(sym),quote(getBlockResult(elem.zero._2))))
        case _ =>
          throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
      }


      loopAnalysisResult.get(currentLoopLevel) match {
        case Some((_,_,SpanOne(_))) =>
          stream.println("int " + quote(op.v) + " = " + getCurrentDimGlobalIdx + ";")
          //stream.println("if (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))

        case _ =>
          stream.println("int " + quote(op.v) + " = " + getCurrentDimIdx + ";")
          stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
      }

      // body
      emitMultiLoopFuncs(op, symList)
      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
          emitValDef(elem.buf.allocVal, quote(sym)+"_data")
          emitBlock(elem.buf.update)
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
          stream.println("//start emitReduceElem")
          emitReduceElem(op, sym, elem)
          stream.println("//end emitReduceElem")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          stream.println("//start emitReduceTupleElem")
          emitReduceTupleElem(op, sym, elem)
          stream.println("//end emitReduceTupleElem")
        case _ =>
          throw new GenerationFailedException("GPUGen: Unsupported Elem Type!")
      }

      loopAnalysisResult.get(currentLoopLevel) match {
        //case Some((_,_,SpanOne(_))) =>
        //  stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
        case Some((_,_,SpanAll)) =>
          stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
        case Some((_,_,SpanOne(_))) =>
          stream.println(quote(op.v) + " += " + getCurrentDimStride + ";")
        case _ => //
      }
      stream.println(/*{*/"} // end fat loop " + symList.map(quote).mkString(","))

      (symList zip op.body) foreach {
        case (sym, elem: DeliteCollectElem[_,_,_]) =>
          emitValDef(elem.buf.allocVal, quote(sym) + "_data")
          emitBlock(elem.buf.finalizer)
          emitValDef(sym, quote(getBlockResult(elem.buf.finalizer)))
        case (sym, elem: DeliteForeachElem[_]) =>
        case (sym, elem: DeliteReduceElem[_]) =>
          // reduce across threads in this dimension
          stream.println("__shared__ " + remap(sym.tp) + " shared_" + quote(sym) + getOuterLoopDimSizes.map("["+_+"]").mkString("") + "[" + getCurrentDimSize + "];")
          stream.println("shared_" + quote(sym) + getOuterLoopDimIndices.map("["+_+"]").mkString("") + "[" + getCurrentDimIdx + "] = " + quote(sym) + ";")
          stream.println("__syncthreads();")
          stream.println("if(" + getCurrentDimIdx + " == 0) { for(int i=1; i<" + getCurrentDimSize + "; i++) { " + quote(sym) + " += shared_" + quote(sym) + getOuterLoopDimIndices.map("["+_+"]").mkString("") + "[i]; } }")
        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
      }
    }
  }

  override def remapInputs(inputs: List[Sym[Any]], size: Exp[Int] = outerLoopSize, v: Sym[Int] = outerLoopSym.asInstanceOf[Sym[Int]]) : List[String] = {
    if (multiDimMapping == false) {
      super.remapInputs(inputs, size, v)
    }
    else {
      // last inputs always added to any device functions
      def lastInputs = (size match {
        case s@Sym(_) => List(size).map(i => remap(i.tp) + " " + quote(i))
        case _ => List("int size")
      }) ++ List("TEMP_"+kernelName+" size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, activation_"+kernelName+" act")

      inputs.filter(_ != size).map(s =>
        if(inVars contains s)
          deviceTarget + "Ref" + remap(s.tp) + " " + quote(s)
        else
          remap(s.tp) + " " + quote(s)
      ) ++ lastInputs
    }
  }

  // emit process functions
  override def emitProcessMethods(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    if (multiDimMapping == false) {
      super.emitProcessMethods(op, symList)
    }
    else {
      emitHashReduceElemProcess(op, symList)
      (symList zip op.body) foreach {
        case (sym, elem:DeliteCollectElem[_,_,_]) =>
          val freeVars = (getFreeVarBlock(Block(Combine((List(elem.func,elem.buf.update,elem.buf.appendable)++elem.cond).map(getBlockResultFull))),List(elem.buf.eV,elem.buf.allocVal,op.v,sym))++List(sym)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars)
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          //emitBlock(elem.func)
          //emitValDef(elem.eV, quote(getBlockResult(elem.func)))
          //if(multiDimMapping)
          //emitValDef(op.v, getCurrentDimGlobalIdx)
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanOne(_))) =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimGlobalIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
            case _ =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          }

          elem.par match {
            case ParSimpleBuffer =>
              //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
              emitFatBlock(elem.cond)
              if (elem.cond.nonEmpty) stream.println("if (" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
              emitBlock(elem.buf.appendable)
              stream.println("if (" + quote(getBlockResult(elem.buf.appendable)) + ") {")
              //emitValDef(elem.allocVal, "act." + quote(sym) + "_buf")
              //emitBlock(elem.update)
              stream.println("act." + quote(sym) + "_bitmap[" + quote(op.v) + "] = 1;")
              stream.println("}")
              if (elem.cond.nonEmpty) {
                // Need this for GPU?
                // stream.println(quote(sym) + "_conditionals[" + quote(op.v) + "] += 1;")
                stream.println("}")
              }
            case ParFlat =>
              emitBlock(elem.func)
              emitValDef(elem.buf.eV, quote(getBlockResult(elem.func)))
              emitValDef(elem.buf.allocVal, quote(sym))
              //if(multiDimMapping)
              //  stream.println("if (" + getInnerLoopDimIndices.map(_ + "==0").mkString("&&") + ") {")
              emitBlock(elem.buf.update)
              //if(multiDimMapping)
              //  stream.println("}")
            case _ =>
          }
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanAll)) =>
              stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
            case Some((_,_,SpanOne(_))) =>
              stream.println(quote(op.v) + " += " + getCurrentDimStride + ";")
            case _ => //
          }
          stream.println("}")
          stream.println("}")

        case (sym, elem:DeliteForeachElem[_]) =>
          val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars)
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ void dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          //emitValDef(op.v, getCurrentDimGlobalIdx)
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanOne(_))) =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimGlobalIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
            case _ =>
              stream.println("int " + quote(op.v) + " = " + getCurrentDimIdx + ";")
              stream.println("while (" + quote(op.v) + " < " + quote(op.size) + ") {  // begin fat loop " + symList.map(quote).mkString(","))
          }
          emitBlock(elem.func)
          loopAnalysisResult.get(currentLoopLevel) match {
            case Some((_,_,SpanAll)) =>
              stream.println(quote(op.v) + " += " + getCurrentDimSize + ";")
            case Some((_,_,SpanOne(_))) =>
              stream.println(quote(op.v) + " += " + getCurrentDimStride + ";")
            case _ => //
          }
          stream.println("}")
          stream.println("}")

        case (sym, elem: DeliteReduceElem[_]) if(encounteredZipWith contains getBlockResult(elem.rFunc)) =>
          val freeVars = getFreeVarBlock(elem.func,List(op.v)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars)
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitValDef(op.v, getCurrentDimGlobalIdx)
          emitBlock(elem.func)
          stream.println("return " + quote(getBlockResult(elem.func)) + ";")
          stream.println("}")

        case (sym, elem:DeliteReduceElem[_]) =>
          val freeVars = getFreeVarBlock(Block(Combine((List(elem.func,elem.rFunc,elem.zero)++elem.cond).map(getBlockResultFull))),List(elem.rV._1,elem.rV._2,op.v)).filter(_ != op.size).distinct
          val inputs = remapInputs(freeVars ++ List(elem.rV._1))
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          stream.println("__device__ " + remap(sym.tp) + " dev_process_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
          emitValDef(op.v, getCurrentDimGlobalIdx)
          emitBlock(elem.func)
          emitValDef(elem.rV._2, quote(getBlockResult(elem.func)))
          if(elem.cond.nonEmpty) {
            emitFatBlock(elem.cond)
            stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
            if (elem.stripFirst) {
              emitBlock(elem.zero)
              stream.println(remap(sym.tp) + " " + quote(sym) + "_zero = " + quote(getBlockResult(elem.zero)) + ";")
              stream.println("if(" + quote(elem.rV._1) + " == " + quote(sym) + "_zero) {")
              stream.println("return " + quote(elem.rV._2) + ";")
              stream.println("}")
              stream.println("else {")
              emitBlock(elem.rFunc)
              stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
              stream.println("}")
            }
            else {
              emitBlock(elem.rFunc)
              stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
            }
            stream.println("}")
            stream.println("else {")
            stream.println("return " + quote(elem.rV._1) + ";")
            stream.println("}")
          }
          else {
            emitBlock(elem.rFunc)
            stream.println("return " + quote(getBlockResult(elem.rFunc)) + ";")
          }
          stream.println("}")

        case (sym, elem: DeliteReduceTupleElem[_,_]) =>
          val freeVars = getFreeVarBlock(Block(Combine((List(elem.func._1,elem.func._2,elem.rFuncSeq._1,elem.rFuncSeq._2)++elem.cond).map(getBlockResultFull))),List(elem.rVSeq._1._1,elem.rVSeq._1._2,elem.rVSeq._2._1,elem.rVSeq._2._2,op.v)).filter(_ != op.size).distinct
          val e = metaData.outputs.get(sym).get
          e.funcs += "process" -> freeVars.map(quote)
          for(i <- 1 until 3) {
            val (rVSeq1, rVSeq2, rFuncSeq) = if(i == 1) (elem.rVSeq._1._1, elem.rVSeq._2._1, elem.rFuncSeq._1)
                                             else (elem.rVSeq._1._2, elem.rVSeq._2._2, elem.rFuncSeq._2)
            val inputs = remapInputs(freeVars ++ List(elem.rVSeq._1._1,elem.rVSeq._1._2))
            stream.println("__device__ " + remap(sym.tp) + " dev_process" + i + "_" + funcNameSuffix(sym) + "(" + inputs.mkString(",") + ") {")
            emitValDef(op.v, getCurrentDimGlobalIdx)
            emitFatBlock(List(elem.func._1,elem.func._2))
            emitValDef(elem.rVSeq._2._1, quote(getBlockResult(elem.func._1)))
            emitValDef(elem.rVSeq._2._2, quote(getBlockResult(elem.func._2)))
            if(elem.cond.nonEmpty) {
              emitFatBlock(elem.cond)
              stream.println("if(" + elem.cond.map(c=>quote(getBlockResult(c))).mkString(" && ") + ") {")
              assert(!elem.stripFirst)
              emitBlock(rFuncSeq)
              stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
              stream.println("}")
              stream.println("else {")
              stream.println("return " + quote(rVSeq1) + ";")
              stream.println("}")
            }
            else {
              emitBlock(rFuncSeq)
              stream.println("return " + quote(getBlockResult(rFuncSeq)) + ";")
            }
            stream.println("}")
          }

        case _ => //
      }
    }
  }


}

trait CudaGenDeliteOps extends CudaGenLoopsFat with GPUGenDeliteOpsOpt with CudaGenDeliteInternalOps

trait OpenCLGenDeliteOps extends OpenCLGenLoopsFat with GPUGenDeliteOpsOpt with OpenCLGenDeliteInternalOps
