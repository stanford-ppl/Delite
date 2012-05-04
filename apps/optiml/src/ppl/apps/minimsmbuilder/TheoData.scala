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

import ppl.dsl.optiml._

trait TheoData extends OptiMLApplication {

  type XYZ = Record{val x: Float
                    val y: Float
                    val z: Float}  
                    
  def lineToXYZ(line: Rep[DenseVector[String]]) = new Record {
    val x = line(0).toFloat
    val y = line(1).toFloat
    val z = line(2).toFloat
  }
  
  type Theo = Record{val XYZData            : DenseVector[XYZ] 
                     val G                  : DenseMatrix[Float]
                     val numAtoms           : Int
                     val numAtomsWithPadding: Int}
                     
  /* 
   Stores temporary data required during Theobald RMSD calculation.

   Notes:
     Storing temporary data allows us to avoid re-calculating the G-Values
     repeatedly. Also avoids re-centering the coordinates. 
  */                     
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
    x.G(index) = value.G // TODO matrix slice updating
  }
  */
  def len(x: Rep[Theo]) = x.XYZData.length
       
}
