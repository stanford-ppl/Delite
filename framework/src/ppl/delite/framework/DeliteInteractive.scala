package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common.{BaseExp, Base, SynchronizedArrayBufferOps}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.Target
import ops.DeliteOpsExp

trait DeliteInteractive extends SynchronizedArrayBufferOps {
  implicit def staticArrayBuffer[A:Manifest](x: ArrayBuffer[A]): Rep[ArrayBuffer[A]]
}

trait DeliteInteractiveRunner extends DeliteApplication with DeliteInteractive {
  def staticArrayBuffer[A:Manifest](x: ArrayBuffer[A]): Rep[ArrayBuffer[A]] = staticData(x)
  
  def apply: Any
  def main = apply
  def run = { 
    val name = "scope-temp"
    Config.degFilename = name
    main(scala.Array())
    ppl.delite.runtime.Delite.embeddedMain(scala.Array(name), staticDataMap) 
  }
  System.out.println("object created")
  run
}

object DeliteSnippet {
  def apply[A,B](b: => Unit) = new Scope[A,B,Unit](b)
}



/*

import ppl.delite.framework._

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala._

import scala.virtualization.lms.common.SynchronizedArrayBufferOps

import scala.collection.mutable.ArrayBuffer

trait OptiMLInteractive extends OptiMLApplication with SynchronizedArrayBufferOps {
  implicit def staticArrayBuffer[A:Manifest](x: ArrayBuffer[A]): Rep[ArrayBuffer[A]]
}

trait OptiMLInteractiveRunner extends OptiMLApplicationRunner with DeliteInteractiveRunner { 
  def staticArrayBuffer[A:Manifest](x: ArrayBuffer[A]): Rep[ArrayBuffer[A]] = staticData(x)
}

def OptiML[R](b: => R) = new Scope[OptiMLInteractive, OptiMLInteractiveRunner, R](b)




OptiML { val v = Vector.ones(5); v.pprint }

val ab = new ArrayBuffer[Vector[Double]]

OptiML { ab += Vector.ones(5) }

println(ab)

OptiML { println(ab) }

*/



/*
scala> trait DSL { def foo: Int }
defined trait DSL

scala> trait Impl { def foo = 7; def apply: Any }
defined trait Impl

scala> def OptiML[R](b: => R) = new Scope[DSL, Impl, R](b)
OptiML: [R](b: => R)Scope[DSL,Impl,R]

scala> val a = OptiML { println("bar") }
a: DSL with Impl with () => Unit = <function0>
*/