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

trait TheoData extends OptiMLApplication {

  type XYZ = Tuple3[Float,Float,Float]
      
  /**
   * parser for reading xyz elements from file
   */
  def lineToXYZ(line: Rep[String]) = {
    val data = line.split(",")
    ((data(0).toFloat, data(1).toFloat, data(2).toFloat))
  }
  
  /* syntactic sugar */
  def infix_x(a: Rep[XYZ]) = a._1
  def infix_y(a: Rep[XYZ]) = a._2
  def infix_z(a: Rep[XYZ]) = a._3
                           
  /* 
   Stores temporary data required during Theobald RMSD calculation.

   Notes:
     Storing temporary data allows us to avoid re-calculating the G-Values
     repeatedly. Also avoids re-centering the coordinates. 
  */                     
  
  type Theo = Record{val XYZData            : DenseMatrix[XYZ] 
                     val G                  : DenseVector[Float]
                     val numAtoms           : Int
                     val numAtomsWithPadding: Int}
  
  /*
  def theo(pathToXYZ: Rep[String], numAtoms: Option[Rep[Int]] = None, G: Option[Rep[DenseMatrix[Float]]] = None) = {
    val v = readVector[XYZ](pathToXYZ, lineToXYZ)
    
    // Create a container for intermediate values during RMSD Calculation.
    // 
    // Notes:
    //   1.  We remove center of mass.
    //   2.  We pre-calculate matrix magnitudes (ConfG)    
  }
  */
   
  // -- helpers  
  def get(x: Rep[Theo], n: Interface[IndexVector]) = {
    // doesn't compile if the applyDynamic field accesses are inside the new record definition...
    val t1 = x.XYZData
    val t2 = x.G
    val t3 = x.numAtoms
    val t4 = x.numAtomsWithPadding
    new Record {
      val XYZData = t1(n)
      val G = t2(n)
      val numAtoms = t3
      val numAtomsWithPadding = t4
    }
  }
  /*
  def set(x: Rep[Theo], index: Rep[IndexVector], value: Rep[Theo]): Rep[Theo] = new Record {
    x.XYZData(index) = value.XYZData
    x.G(index) = value.G 
  }
  */
  def len(x: Rep[Theo]) = x.XYZData.numRows
       
}
