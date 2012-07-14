package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OPData, Targets}
import ppl.delite.runtime.scheduler.OpList

/* trait CudaGPUExecutableGenerator extends GPUExecutableGenerator {

  protected def emitCppBody(schedule: OpList, location: Int, syncList: ArrayBuffer[DeliteOP]): String = {
    val out = new StringBuilder //the output string
    implicit val aliases = new AliasTable[(DeliteOP,String)]

    //the JNI method
    writeFunctionHeader(location, out)

    //initialize
    writeGlobalsInitializer(out)
    val locations = Range.inclusive(0,location).toSet
    writeJNIInitializer(locations, out)

    //execute
    addKernelCalls(schedule, location, new ArrayBuffer[(DeliteOP,String)], new ArrayBuffer[DeliteOP], syncList, out)

    writeJNIFinalizer(locations, out)
    out.append('}')
    out.append('\n')

    out.toString
  }


  protected def addKernelCalls(schedule: OpList, location: Int, available: ArrayBuffer[(DeliteOP,String)], awaited: ArrayBuffer[DeliteOP], syncList: ArrayBuffer[DeliteOP], out: StringBuilder)(implicit aliases:AliasTable[(DeliteOP,String)]) {
    //available: list of ops with data currently on gpu, have a "g" symbol
    //awaited: list of ops synchronized with but data only resides on cpu, have a "c" symbol
    val getterList = new ArrayBuffer[String]
    for (op <- schedule) {
      //add to available & awaited lists
      if (op.isInstanceOf[OP_Nested])
        available += Pair(op,op.id.replaceAll("_"+op.scheduledResource,""))
      else
        available ++= op.getGPUMetadata(target).outputs.map(o=>Pair(op,o._2))

      awaited += op

      if (op.isInstanceOf[OP_Nested]) makeNestedFunction(op, location)

      //get all dependencies
      for (dep <- op.getDependencies) { //foreach dependency
        if(!awaited.contains(dep)) { //this dependency does not yet exist for this resource
          awaited += dep
          for (sym <- dep.getOutputs) //TODO: should get and set outputs individually; SMP needs to adopt this strategy as well
            writeGetter(dep, sym, location, getterList, out) //get to synchronize
        }
      }
      //get kernel inputs (dependencies that could require a memory transfer)
      var addInputCopy = false
      for ((input, sym) <- op.getInputs) { //foreach input
        val inData = op.getGPUMetadata(target).inputs.getOrElse((input, sym), null)
        if(!available.contains(input,sym)) { //this input does not yet exist on the device
          //add to available list
          available += Pair(input,sym)
          //write a copy function for objects
          if (inData != null) { //only perform a copy for object types
            addInputCopy = true
            writeInputCopy(input, sym, inData.func, inData.resultType, out, true)
          }
          else if (isPrimitiveType(input.outputType(sym)))
            writeInputCast(input, sym, out) //if primitive type, simply cast to transform from "c" type into "g" type
          else {
            assert(op.isInstanceOf[OP_Nested],op.id+":cuda metadata for output copy not specified") //object without copy must be for a nested function call
            available -= Pair(input,sym) //this input doesn't actually reside on GPU
          }
        }
        else if (needsUpdate(op, input, sym)) { //input exists on device but data is old
          //write a new copy function (input must be an object)
          addInputCopy = true
          writeInputCopy(input, sym, inData.func, inData.resultType, out, false)
        }
      }
      if (addInputCopy) { //if a h2d data transfer occurred
        //sync kernel launch with completion of last input copy
        out.append("addEvent(h2dStream, kernelStream);\n")
      }

      //write the temporary allocations
      writeTempAllocs(op, out)
      //write the output allocation
      writeOutputAllocs(op, out)
      //write the call
      op match {
        case n: OP_Nested => writeFunctionCall(op, out)
        case e: OP_External => writeLibraryCall(op, out)
        case m: OP_MultiLoop => writeMultiKernelCall(op, out)
        case _ => writeKernelCall(op, out)
      }

      //write the setter
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true
      }
      if (addSetter) {
        syncList += op //add op to list that needs sync generation
        //sync output copy with kernel completion
        out.append("addEvent(kernelStream, d2hStream);\n")
        //write a setter
        writeSetters(op, location, out)
      }
      writeDataFrees(op, out, available)
    }
    writeGetterFrees(getterList, out)
  }

  //TODO: should track if data has already been updated - implement current version for each resource - useful for gpu/cluster
  //TODO: updates don't work across loop iterations (write seen as happening after rather than before)
  protected def needsUpdate(op: DeliteOP, input: DeliteOP, sym: String): Boolean = {
    for (dep <- op.getDependencies) {
      if (dep.getMutableInputs.contains(input, sym) && dep.scheduledResource != op.scheduledResource) {
        return true
      }
    }
    false
  }

} */

