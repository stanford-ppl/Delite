package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/28/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/*
 * MetaInteger.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

trait MetaInteger {
  abstract class IntM {
    type Succ <: IntM
    type Add[N <: IntM] <: IntM
  }

  class Zero extends IntM {
    type Succ = MetaInteger.this.Succ[Zero]
    type Add[N <: IntM] = N
  }
  class Succ[N <: IntM] extends IntM {
    type Succ = MetaInteger.this.Succ[MetaInteger.this.Succ[N]]
    type Add[M <: IntM] = (N#Add[M])#Succ
  }

  type +[N1 <: IntM, N2 <: IntM] = N1#Add[N2]

  //Scala doesn't provide meta-integers like C++'s typename<int N>
  //so we have to define some integers here
  //this is a simplification of:
  //http://michid.wordpress.com/code/meta-programming-with-scala-part-i-addition/
  //somelike like Church encoding but the numbers are themselves objects and not
  //functions for simplicity since we don't need to define addition on them
  //as far I can tell ... if necessary I suppose we can define multiplcation
  //on these if we needed numbers higher than 9
  type _0 = Zero
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]
  type _16 = Succ[_15]
  type _17 = Succ[_16]
  type _18 = Succ[_17]
  type _19 = Succ[_18]
  type _20 = Succ[_19]
  type _21 = Succ[_20]
  type _22 = Succ[_21]

  //meta-function(?)(what is the term for this thing?)
  //if IDX is less than SIZE, then an implicit object of type EnsureSize[IDX,SIZE] will
  //exist with a idx function that returns the number IDX as an integer
  abstract class EnsureSize[TT <: IntM,T <: IntM] {
    def get(n : Int) : Int
    def idx = get(0)
  }

  implicit def EnsureSizeZero[T <: IntM] =
  new EnsureSize[Zero,Succ[T]] {
    def get(n : Int) : Int = n
  }

  implicit def EnsureSizeSucc[T <: IntM, TT <: IntM](implicit fn : EnsureSize[TT,T]) =
  new EnsureSize[Succ[TT],Succ[T]] {
    def get(n : Int) : Int = fn.get(n+1)
  }

  abstract class MIntToInt[T] {
    val value: Int
  }

  implicit val MIntToIntZero = new MIntToInt[Zero] {
    val value = 0
  }

  implicit def MIntToIntSucc[T <: IntM](implicit f: MIntToInt[T]) = new MIntToInt[Succ[T]] {
    val value = f.value + 1
  }

  def MIntDepth[T <: IntM](implicit n: MIntToInt[T]) = n.value
}

object MetaInteger extends MetaInteger {
  //for convience we provide objects representing each number as well
  //so you can avoid passing parameter arguments
  val _0 : _0 = new _0
  val _1 : _1 = new _1
  val _2 : _2 = new _2
  val _3 : _3 = new _3
  val _4 : _4 = new _4
  val _5 : _5 = new _5
  val _6 : _6 = new _6
  val _7 : _7 = new _7
  val _8 : _8 = new _8
  val _9 : _9 = new _9
  val _10 : _10 = new _10
  val _11 : _11 = new _11
  val _12 : _12 = new _12
  val _13 : _13 = new _13
  val _14 : _14 = new _14
  val _15 : _15 = new _15
  val _16 : _16 = new _16
  val _17 : _17 = new _17
  val _18 : _18 = new _18
  val _19 : _19 = new _19
  val _20 : _20 = new _20
  val _21 : _21 = new _21
  val _22 : _22 = new _22
}