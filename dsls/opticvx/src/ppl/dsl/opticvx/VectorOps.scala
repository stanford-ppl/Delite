package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import ppl.dsl.optila

import scala.reflect.SourceContext

import java.io.PrintWriter


trait VectorOps extends Base {
  
}


trait VectorOpsExp extends VectorOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with StringOpsExp with WhileExp with VariablesExp
        with optila.OptiLAExp =>
  
  type CVXVector = optila.DenseVector[Double] //Array[Double]
  
  implicit val implicit_dense_vector_builder = denseVectorBuilder[Double]
  
  def infix_+(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = vector_sum(x,y)
  
  //sum of two vectors
  def vector_sum(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector] = {
    if(densevector_length[Double](x) == unit(0)) {
      densevector_obj_zeros(unit(0))
    }
    else {
      vector_plus[Double,optila.DenseVector[Double]](x,y)
    }
  }

  //negation of a vector
  def vector_neg(x: Exp[CVXVector]): Exp[CVXVector] = {
    if(densevector_length[Double](x) == unit(0)) {
      densevector_obj_zeros(unit(0))
    }
    else {
      vector_times_scalar[Double,optila.DenseVector[Double]](x,unit(-1.0))
    }
  }

  //positive part of a vector
  def vector_positive_part(x: Exp[CVXVector]): Exp[CVXVector] = {
    if(densevector_length[Double](x) == unit(0)) {
      densevector_obj_zeros(unit(0))
    }
    else {
      vector_map[Double,Double,optila.DenseVector[Double]](x, (a: Exp[Double]) => math_max(a,unit(0.0)))
    }
  }
  
  //vector is positive
  def vector_ispositive(x: Exp[CVXVector]): Exp[Boolean] = {
    vector_minimum(x) >= unit(0.0)
  }
  
  //minimum value of a vector
  def vector_minimum(x: Exp[CVXVector]): Exp[Double] = {
    if(densevector_length[Double](x) == unit(0)) {
      Double.PositiveInfinity
    }
    else {
      vector_min[Double](x)
    }
  }
    
  //scale of a vector
  def vector_scale(x: Exp[CVXVector], s: Exp[Double]): Exp[CVXVector] = {
    if(densevector_length[Double](x) == unit(0)) {
      densevector_obj_zeros(unit(0))
    }
    else {
      vector_times_scalar[Double,optila.DenseVector[Double]](x,s)
    }
  }
    
  //dot product of two vectors
  def vector_dot(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[Double] = {
    if(densevector_length[Double](x) == unit(0)) {
      unit(0.0)
    }
    else {
      vector_dot_product[Double](x,y)
    }
  }
    
  //select a subrange of values from a vector
  def vector_select(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]): Exp[CVXVector] = {
    vector_slice[Double,optila.DenseVector[Double]](x,offset,offset+len)
  }

  //concatenate two vectors
  def vector_cat(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector] = {
    vector_concatenate[Double,optila.DenseVector[Double]](x,y)
  }

  //create a zero-vector
  def vector_zeros(len: Exp[Int]): Exp[CVXVector] = {
    densevector_obj_zeros(len)
  }
    
  //create a ones-vector
  def vector_ones(len: Exp[Int]): Exp[CVXVector] = {
    densevector_obj_ones(len)
  }
  
  //create a size-1 vector with a particular value
  def vector1(u: Exp[Double]): Exp[CVXVector] = {
    densevector_obj_fromseq[Double](Seq(u))
  }
  
  def vector_fromseq(us: Exp[Seq[Double]]): Exp[CVXVector] = {
    densevector_obj_fromseq[Double](us)
  }
    
  //index the vector at the given index
  def vector_at(x: Exp[CVXVector], i: Exp[Int]): Exp[Double] = {
    densevector_apply[Double](x,i)
  }

  //find the length of the vector
  def vector_len(x: Exp[CVXVector]): Exp[Int] = {
    densevector_length[Double](x)
  }
    
  //convert a vector to matlab string representation (DEBUG)
  //case class VectorToStringMatlab(x: Exp[CVXVector]) extends Def[String]
  def vector_to_string_matlab(x: Exp[CVXVector]): Exp[String] = {
    val vi = var_new[Int](Const(0))
    val vacc = var_new[String](Const("["))
    __whileDo(readVar(vi) < vector_len(x) - Const(1), {
      var_assign(vacc, readVar(vacc) + string_valueof(vector_at(x,readVar(vi))) + Const(", "))
      var_assign(vi, readVar(vi) + Const(1))
    })
    var_assign(vacc, readVar(vacc) + string_valueof(vector_at(x,vector_len(x)-Const(1))) + Const("]"))
    readVar(vacc)
  }
  
}


/*
trait VectorOpsExp extends VectorOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with StringOpsExp with WhileExp with VariablesExp
        with optila.vector.VectorOpsExp =>
  
  type CVXVector = Array[Double]
  
  def infix_+(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = vector_sum(x,y)
  
  //sum of two vectors
  case class VectorSumExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[CVXVector]
  def vector_sum(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = VectorSumExp(x,y)

  //negation of a vector
  case class VectorNegExp(x: Exp[CVXVector]) extends Def[CVXVector]
  def vector_neg(x: Exp[CVXVector]): Exp[CVXVector]
    = VectorNegExp(x)

  //negation of a vector
  case class VectorPositivePartExp(x: Exp[CVXVector]) extends Def[CVXVector]
  def vector_positive_part(x: Exp[CVXVector]): Exp[CVXVector]
    = VectorPositivePartExp(x)
    
  //negation of a vector
  case class VectorIsPositiveExp(x: Exp[CVXVector]) extends Def[Boolean]
  def vector_ispositive(x: Exp[CVXVector]): Exp[Boolean]
    = VectorIsPositiveExp(x)
    
  //scale of a vector
  case class VectorScaleExp(x: Exp[CVXVector], s: Exp[Double]) extends Def[CVXVector]
  def vector_scale(x: Exp[CVXVector], s: Exp[Double]): Exp[CVXVector]
    = VectorScaleExp(x,s)
    
  //dot product of two vectors
  case class VectorDotExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[Double]
  def vector_dot(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[Double]
    = VectorDotExp(x,y)
    
  //select a subrange of values from a vector
  case class VectorSelectExp(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]) extends Def[CVXVector]
  def vector_select(x: Exp[CVXVector], offset: Exp[Int], len: Exp[Int]): Exp[CVXVector]
    = VectorSelectExp(x,offset,len)

  //concatenate two vectors
  case class VectorCatExp(x: Exp[CVXVector], y: Exp[CVXVector]) extends Def[CVXVector]
  def vector_cat(x: Exp[CVXVector], y: Exp[CVXVector]): Exp[CVXVector]
    = VectorCatExp(x,y)

  //create a zero-vector
  case class VectorZeros(len: Exp[Int]) extends Def[CVXVector]
  def vector_zeros(len: Exp[Int]): Exp[CVXVector]
    = VectorZeros(len)
    
  //create a ones-vector
  case class VectorOnes(len: Exp[Int]) extends Def[CVXVector]
  def vector_ones(len: Exp[Int]): Exp[CVXVector]
    = VectorOnes(len)

  //create a size-1 vector with a particular value
  case class Vector1(u: Exp[Double]) extends Def[CVXVector]
  def vector1(u: Exp[Double])
    = Vector1(u)
    
  case class VectorAt(x: Exp[CVXVector], i: Exp[Int]) extends Def[Double]
  def vector_at(x: Exp[CVXVector], i: Exp[Int]): Exp[Double]
    = VectorAt(x,i)

  case class VectorLen(x: Exp[CVXVector]) extends Def[Int]
  def vector_len(x: Exp[CVXVector]): Exp[Int]
    = VectorLen(x)
    
  //convert a vector to matlab string representation (DEBUG)
  //case class VectorToStringMatlab(x: Exp[CVXVector]) extends Def[String]
  def vector_to_string_matlab(x: Exp[CVXVector]): Exp[String] = {
    val vi = var_new[Int](Const(0))
    val vacc = var_new[String](Const("["))
    __whileDo(readVar(vi) < vector_len(x) - Const(1), {
      var_assign(vacc, readVar(vacc) + string_valueof(vector_at(x,readVar(vi))) + Const(", "))
      var_assign(vi, readVar(vi) + Const(1))
    })
    var_assign(vacc, readVar(vacc) + string_valueof(vector_at(x,vector_len(x)-Const(1))) + Const("]"))
    readVar(vacc)
  }
}
*/

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    super.emitNode(sym, rhs)
    /*
    rhs match {
      case VectorSumExp(x,y) =>
        stream.println("if(" + quote(x) + ".length != " + quote(y) + ".length) {")
        for(sc <- sym.pos) {
          stream.println("println(\"At " + sc.fileName + ": " + sc.line + " (" + sc.charOffset + ")\")")
        }
        stream.println("println(\"With x = " + findDefinition(x.asInstanceOf[Sym[_]]).toString() + "\")")
        stream.println("println(\"With y = " + findDefinition(y.asInstanceOf[Sym[_]]).toString() + "\")")
        stream.println("throw new Exception(\"OptiCVX Runtime Error: Vector length mismatch on sum (\" + " + quote(x) + ".length + \" vs \" + " + quote(y) + ".length + \").\")")
        stream.println("}")
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length)")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = " + quote(x) + "(i) + " + quote(y) + "(i)")
        stream.println("}")
        
      case VectorDotExp(x,y) =>
        stream.println("if(" + quote(x) + ".length != " + quote(y) + ".length)")
        stream.println("throw new Exception(\"OptiCVX Runtime Error: Vector length mismatch on dot product (\" + " + quote(x) + ".length + \" vs \" + " + quote(y) + ".length + \").\")")
        stream.println("var acc" + quote(sym) + ": Double = 0.0")
        stream.println("for(i <- 0 until " + quote(x) + ".length) {")
        stream.println("acc" + quote(sym) + " += " + quote(x) + "(i) * " + quote(y) + "(i)")
        stream.println("}")
        stream.println("val " + quote(sym) + " = acc" + quote(sym))

      case VectorNegExp(x) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length)")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = -" + quote(x) + "(i)")
        stream.println("}")
        
      case VectorPositivePartExp(x) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length)")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = Math.max(0.0," + quote(x) + "(i))")
        stream.println("}")
        
      case VectorIsPositiveExp(x) =>
        stream.println("var acc" + quote(sym) + " = true")
        stream.println("for(elem" + quote(sym) + " <- " + quote(x) + ") {")
        stream.println("acc" + quote(sym) + " &&= (elem" + quote(sym) + " >= 0.0)")
        stream.println("}")
        stream.println("val " + quote(sym) + " = acc" + quote(sym))
        
      case VectorScaleExp(x,s) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length)")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = " + quote(x) + "(i) * (" + quote(s) + ")")
        stream.println("}")
        
      case VectorSelectExp(x, offset, len) =>
        stream.println("val " + quote(sym) + " = " + quote(x) + ".slice(" + quote(offset) + ", " + quote(offset) + "+" + quote(len) + ")")
        //stream.println("val " + quote(sym) + " = new Array[Double](" + quote(len) + ")")
        //stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        //stream.println(quote(sym) + "(i) = " + quote(x) + "(i + " + quote(offset) + ")")
        //stream.println("}")
        
      case VectorCatExp(x, y) =>
        stream.println("val " + quote(sym) + " = " + quote(x) + "++" + quote(y))
        //stream.println("val " + quote(sym) + " = new Array[Double](" + quote(x) + ".length + " + quote(y) + ".length)")
        //stream.println("for(i <- 0 until " + quote(x) + ".length) {")
        //stream.println(quote(sym) + "(i) = " + quote(x) + "(i)")
        //stream.println("}")
        //stream.println("for(i <- 0 until " + quote(y) + ".length) {")
        //stream.println(quote(sym) + "(i + " + quote(x) + ".length) = " + quote(y) + "(i)")
        //stream.println("}")
        
      case VectorZeros(len) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(len) + ")")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = 0.0")
        stream.println("}")
                
      case VectorOnes(len) =>
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(len) + ")")
        stream.println("for(i <- 0 until " + quote(sym) + ".length) {")
        stream.println(quote(sym) + "(i) = 1.0")
        stream.println("}")
        
      case Vector1(u) =>
        stream.println("val " + quote(sym) + " = new Array[Double](1)")
        stream.println(quote(sym) + "(0) = " + quote(u))
        
      case VectorAt(x, i) =>
        stream.println("val " + quote(sym) + " = " + quote(x) + "(" + quote(i) + ")")

      case VectorLen(x) =>
        stream.println("val " + quote(sym) + " = " + quote(x) + ".length")
        
      //case VectorToStringMatlab(x) =>
      //  stream.println("var stracc = \"[\"")
      //  stream.println("for(i <- 0 until " + quote(x) + ".length-1) {")
      //  stream.println("stracc = stracc + " + quote(x) + "(i).toString() + \", \"")
      //  stream.println("}")
      //  stream.println("stracc = stracc + " + quote(x) + "(" + quote(x) + ".length-1) + \"]\"")
      //  stream.println("val " + quote(sym) + " = stracc")
        
      case _ => 
        super.emitNode(sym, rhs)
    }
    */
  }
}
