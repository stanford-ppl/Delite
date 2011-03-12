package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen,GenerationFailedException}

trait DeliteIfThenElseExp extends IfThenElseExp with DeliteOpsExp {

  this: DeliteOpsExp =>

  case class DeliteIfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends DeliteOpCondition[T]

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = cond match {
      // TODO: need to handle vars differently, this could be unsound
    case Const(true) => thenp
    case Const(false) => elsep
    case _ =>
//      val a = reifyEffectsHere(thenp)
//      val b = reifyEffectsHere(elsep)
//      (a,b) match {
//        case (Def(Reify(_,_,_)), _) | (_, Def(Reify(_,_,_))) => reflectEffect(DeliteIfThenElse(cond,a,b))
//        case _ => DeliteIfThenElse(cond, a, b)
//      }
      val a = reifyEffectsHere(thenp)
      val b = reifyEffectsHere(elsep)
      val ae = summarizeEffects(a)
      val be = summarizeEffects(b)
      reflectEffect(DeliteIfThenElse(cond,a,b), ae orElse be)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Reflect(DeliteIfThenElse(c,a,b), u, es) => reflectMirrored(Reflect(DeliteIfThenElse(f(c),f(a),f(b)), mapOver(f,u), f(es)))
    case DeliteIfThenElse(c,a,b) => DeliteIfThenElse(f(c),f(a),f(b))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait DeliteBaseGenIfThenElse extends GenericNestedCodegen {
  val IR: DeliteIfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DeliteIfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteIfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }
}

trait DeliteScalaGenIfThenElse extends ScalaGenEffect with DeliteBaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    /**
     * IfThenElse generates methods for each branch due to empirically discovered performance issues in the JVM
     * when generating long blocks of straight-line code in each branch.
     */
    case DeliteIfThenElse(c,a,b) =>
      val save = deliteKernel
      deliteKernel = false
      stream.println("val " + quote(sym) + " = {")
      stream.println("def " + quote(sym) + "thenb(): " + remap(getBlockResult(a).Type) + " = {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("}")

      stream.println("def " + quote(sym) + "elseb(): " + remap(getBlockResult(b).Type) + " = {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

      stream.println("if (" + quote(c) + ") {")
      stream.println(quote(sym) + "thenb()")
      stream.println("} else {")
      stream.println(quote(sym) + "elseb()")
      stream.println("}")
      stream.println("}")
      deliteKernel = save

    case _ => super.emitNode(sym, rhs)
  }
}


trait DeliteCudaGenIfThenElse extends CudaGenEffect with DeliteBaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case DeliteIfThenElse(c,a,b) =>
          // TODO: Not GPUable if the result is not primitive types.
          // TODO: Changing the reference of the output is dangerous in general.
          // TODO: In the future, consider passing the object references to the GPU kernels rather than copying by value.
          // Below is a safety check related to changing the output reference of the kernel.
          // This is going to be changed when above TODOs are done.
          //if( (sym==kernelSymbol) && (isObjectType(sym.Type)) ) throw new RuntimeException("CudaGen: Changing the reference of output is not allowed within GPU kernel.")

          /*
          val objRetType = (!isVoidType(sym.Type)) && (!isPrimitiveType(sym.Type))
          objRetType match {
            case true => throw new GenerationFailedException("CudaGen: If-Else cannot return object type.")
            case _ =>
          }
          isVoidType(sym.Type) match {
            case true =>
              stream.println(addTab() + "if (" + quote(c) + ") {")
              tabWidth += 1
              emitBlock(a)
              tabWidth -= 1
              stream.println(addTab() + "} else {")
              tabWidth += 1
              emitBlock(b)
              tabWidth -= 1
              stream.println(addTab()+"}")
            case false =>
              stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
              stream.println(addTab() + "if (" + quote(c) + ") {")
              tabWidth += 1
              emitBlock(a)
              stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(a))))
              tabWidth -= 1
              stream.println(addTab() + "} else {")
              tabWidth += 1
              emitBlock(b)
              stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(b))))
              tabWidth -= 1
              stream.println(addTab()+"}")
          }
          */
          val objRetType = (!isVoidType(sym.Type)) && (!isPrimitiveType(sym.Type))
          objRetType match {
            case true =>   //TODO: Remove this case
              //Least check
              (kernelSymbol==sym) match {
                case true => throw new GenerationFailedException("CudaGen: If-Else cannot return object type as kernel output.")
                case _ =>
              }
              //stream.println(addTab() + "%s %s;".format(remap(sym.Type),quote(sym)))
              (sym.Type.typeArguments.length>0) && (isPrimitiveType(sym.Type.typeArguments(0))) match {
                case false => throw new GenerationFailedException("CudaGen: If-Else at least needs to have primitive types for the resulting object elements.")
                case true =>
              }
              val outLocalVar = getNewLocalVar()
              val nextDimStr = getNextDimStr()
              stream.println(addTab() + "%s %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar))
              stream.println(addTab() + "if (" + quote(c) + ") {")
              tabWidth += 1
              emitBlock(a)
              stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(a))))
              stream.println(addTab() + "%s = %s;".format(outLocalVar,getLocalVar(getBlockResult(a),nextDimStr)))
              tabWidth -= 1
              stream.println(addTab() + "} else {")
              tabWidth += 1
              emitBlock(b)
              stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(b))))
              stream.println(addTab() + "%s = %s;".format(outLocalVar,getLocalVar(getBlockResult(b),nextDimStr)))
              tabWidth -= 1
              stream.println(addTab()+"}")
              saveLocalVar(sym,nextDimStr,outLocalVar)
			        allocReference(sym,getBlockResult(a).asInstanceOf[Sym[_]])
            case _ =>
              isVoidType(sym.Type) match {
                case true =>
                  stream.println(addTab() + "if (" + quote(c) + ") {")
                  tabWidth += 1
                  emitBlock(a)
                  tabWidth -= 1
                  stream.println(addTab() + "} else {")
                  tabWidth += 1
                  emitBlock(b)
                  tabWidth -= 1
                  stream.println(addTab()+"}")
                case false =>
                  stream.println(addTab() + "%s %s;".format(remap(sym.Type),quote(sym)))
                  stream.println(addTab() + "if (" + quote(c) + ") {")
                  tabWidth += 1
                  emitBlock(a)
                  stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(a))))
                  tabWidth -= 1
                  stream.println(addTab() + "} else {")
                  tabWidth += 1
                  emitBlock(b)
                  stream.println(addTab() + "%s = %s;".format(quote(sym),quote(getBlockResult(b))))
                  tabWidth -= 1
                  stream.println(addTab()+"}")
              }
          }

        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait DeliteCGenIfThenElse extends CGenEffect with DeliteBaseGenIfThenElse {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case DeliteIfThenElse(c,a,b) =>
          //TODO: using if-else does not work
          remap(sym.Type) match {
            case "void" =>
              stream.println("if (" + quote(c) + ") {")
              emitBlock(a)
              stream.println("} else {")
              emitBlock(b)
              stream.println("}")
            case _ =>
              stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
              stream.println("if (" + quote(c) + ") {")
              emitBlock(a)
              stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
              stream.println("} else {")
              emitBlock(b)
              stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
              stream.println("}")
          }
          /*
          val booll = remap(sym.Type).equals("void")
          if(booll) {
            stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
            stream.println("} else {")
            emitBlock(b)
            stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
            stream.println("}")
          }
          else {
            stream.println("if (" + quote(c) + ") {")
            emitBlock(a)
            stream.println("} else {")
            emitBlock(b)
            stream.println("}")
          }
          */
        case _ => super.emitNode(sym, rhs)
      }
    }
}
