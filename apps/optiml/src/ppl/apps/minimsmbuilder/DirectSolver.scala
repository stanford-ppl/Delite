package ppl.apps.minimsmbuilder

/*
 * This file is part of a port of MSMBuilder.
 * 
 * Copyright 2011 Stanford University
 * 
 * MSMBuilder is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 */

import reflect.{Manifest, SourceContext}
import ppl.dsl.optiml._


//
//=============================================================================================
// Calculation of RMSD by a the quaternion-based characteristic polynomial (QCP) algorithm of Theobald [1].
// 
// [1] Theobald DL. Rapid calculation of RMSDs using a quaternion-based characteristic polynomial. 
//     Acta Cryst., A61:478, 2005.  doi:10.1107/50108767305015266
//
// Written by John D. Chodera <jchodera@gmail.com>, Dill lab, UCSF, 2006.
// Contributions in 2010 from:
//      Kyle Beauchamp(kyleb@stanford.edu)
//      Peter Kasson (kasson@stanford.edu)
//      Kai Kohlhoff (kohlhoff@stanford.edu)
//      Imran Haque  (ihaque@cs.stanford.edu)
//=============================================================================================


/*------------------------------------------------------------------------------
 * The quartic and cubic functions are taken from:
 * FILE: quartic.c
 *
 * AUTHOR: Jonathan Zrake, NYU CCPP: zrake@nyu.edu
 *         Adapted from the nvwa code by Weiqun Zhang
 * Modified by KAB 2011
 * GPLv2 / LGPL exemption from Jonathan Zrake, Aug. 2, 2011
 * Original code from http://code.google.com/p/python-mhd/
 *------------------------------------------------------------------------------
 */

trait DirectSolver extends OptiMLApplication { 

  type QuarticResult = Record{val r1: Double; val r2: Double; val r3: Double; val r4: Double; val nr12: Int; val nr34: Int}
  def NewQuarticResult(_r1: Rep[Double], _r2: Rep[Double], _r3: Rep[Double], _r4: Rep[Double], _nr12: Rep[Int], _nr34: Rep[Int], _ans: Rep[Int]) = {
    new Record {
      val r1 = _r1
      val r2 = _r2
      val r3 = _r3
      val r4 = _r4
      val nr12 = _nr12
      val nr34 = _nr34
      val ans = _ans
    }
  }
  
  type CubicResult = Record{val x1: Double; val x2: Double; val x3: Double; val nr: Int}
  def NewCubicResult(_x1: Rep[Double], _x2: Rep[Double], _x3: Rep[Double], _nr: Rep[Int]) = {
    new Record {
      val x1 = _x1
      val x2 = _x2
      val x3 = _x3
      val nr = _nr
    }
  }                             

  def quarticEquationSolveExact(d0: Rep[Double], d1: Rep[Double], d2: Rep[Double], d3: Rep[Double], d4: Rep[Double]): Rep[QuarticResult] = {                                                          
    val a3 = d3/d4
    val a2 = d2/d4  
    val a1 = d1/d4
    val a0 = d0/d4

    val au2 = -1.*a2
    val au1 = (a1*a3 - 4.0*a0) 
    val au0 = 4.0*a0*a2 - a1*a1 - a0*a3*a3

    val cubicResult = solveCubicEquation(1.0, au2, au1, au0)
    val nr = cubicResult.nr
    val x1 = cubicResult.x1
    val x2 = cubicResult.x2
    val x3 = cubicResult.x3

    val u1 = if (nr==1) x1 else if (x1 > x3) x1 else x3 
    val R2 = 0.25*a3*a3 + u1 - a2
    val R = if (R2 > 0.0) sqrt(R2) else 0.0

    val (dd2, e2) = t2 {
      if (R != 0.0) {
        val foo1 = 0.75*a3*a3 - R2 - 2.0*a2
        val foo2 = 0.25*(4.0*a3*a2 - 8.0*a1 - a3*a3*a3) / R
        (foo1 + foo2, foo1 - foo2)
      } 
      else {
        val foo1 = 0.75*a3*a3 - 2.0*a2
        val foo2 = 2.0 * sqrt(u1*u1 - 4.0*a0)
        (foo1 + foo2, foo1 - foo2)
      }
    }
    
    val (r1,r2,nr12) = t3 {
      if (dd2 >= 0.0) {
        val D = sqrt(dd2)
        (-0.25*a3 + 0.5*R - 0.5*D, -0.25*a3 + 0.5*R + 0.5*D, unit(2))  // FIXME AKS -- not auto-lifting individual elements of tuple returns
      }
      else {
        val t = -0.25*a3 + 0.5*R
        (t, t, unit(0))
      }
    }

    val (r3,r4,nr34) = t3 {
      if (e2 >= 0.0) {
        val E = sqrt(e2)
        (-0.25*a3 - 0.5*R - 0.5*E, -0.25*a3 - 0.5*R + 0.5*E, unit(2))  // FIXME AKS
      }
      else {
        val t = -0.25*a3 - 0.5*R
        (t, t, unit(0))
      }
    }
  
    NewQuarticResult(r1,r2,r3,r4,nr12,nr34,nr12+nr34)
  }

  def solveCubicEquation(c3: Rep[Double], c2: Rep[Double], c1: Rep[Double], c0: Rep[Double]): Rep[CubicResult] = {
    val a2 = c2/c3
    val a1 = c1/c3
    val a0 = c0/c3

    val q = a1/3.0 - a2*a2/9.0
    val r = (a1*a2 - 3.0*a0)/6.0 - a2*a2*a2 / 27.0
    val delta = q*q*q + r*r

    if (delta>0.0) {
      var s1 = r + sqrt(delta)
      s1 = if (s1>=0.0) pow(s1,1./3.) else -1.*pow(s1*(-1.),1./3.)  // FIXME AKS: unary_- with vars

      var s2 = r - sqrt(delta)
      s2 = if (s2>=0.0) pow(s2,1./3.) else -1.*pow(s2*(-1.),1./3.)

      val t = -0.5 * (s1+s2) - a2/3.0
      NewCubicResult((s1+s2) - a2/3.0, t, t, 1)
    }
    else if (delta < 0.0) {
      val theta = acos(r/sqrt(-1.0*q*q*q)) / 3.0
      val costh = cos(theta)
      val sinth = sin(theta)
      val sq = sqrt(-1.0*q)

      NewCubicResult(2.0*sq*costh - a2/3.0, -1.0*sq*costh - a2/3.0 - sqrt(3.) * sq * sinth, -1.0*sq*costh - a2/3.0 + sqrt(3.) * sq * sinth, 3)
    }
    else {
      val s = if (r>=0.0) pow(r,1./3.) else -1.0*pow(-1.*r,1./3.)
      
      val t = -1.*s - a2/3.0      
      NewCubicResult(2.0*s - a2/3.0, t, t, 3)
    }
  }

  def directSolve(lambda: Rep[Double], c0: Rep[Double], c1: Rep[Double], c2: Rep[Double]): Rep[Double] = {
    val q = quarticEquationSolveExact(c0,c1,c2,0.0,1.0)
    var result=max(q.r1,q.r2)
    result=max(result,q.r3)
    result=max(result,q.r4)

    result
  }
}