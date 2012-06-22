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

object MiniMSMApplicationRunner extends OptiMLApplicationRunner with Clarans 
  
trait Clarans extends OptiMLApplication with TheoData with DirectSolver {
  lazy val verbose = true
  
  def vprint(s: Rep[String]) = if (verbose) print(s)
  
  /**
   * Compute the rigid-body aligned RMSD between two sets of vectors
   * x and y should be NAtoms x 3 two-dimensional arrays
   */  
  def rmsd_centered(realLen: Rep[Int], xCentered: Interface[Vector[XYZ]], yCentered: Interface[Vector[XYZ]], gx: Rep[Float], gy: Rep[Float]) = {
    // compute the inner product matrix 
    // println("+++STAGING MARKER: entering perf critical")
    val mX = (0::3, 0::realLen) { (i,j) => if (i == 0) xCentered(j).x else if (i == 1) xCentered(j).y else xCentered(j).z } // inline transpose
    val mY = (0::realLen, 0::3) { (i,j) => if (j == 0) yCentered(i).x else if (j == 1) yCentered(i).y else yCentered(i).z }
    val M = mX * mY
    
    // form the 4x4 symmetric Key matrix K
    val k00 = M(0,0) + M(1,1) + M(2,2)
    val k01 = M(1,2)-M(2,1)
    val k02 = M(2,0)-M(0,2)
    val k03 = M(0,1)-M(1,0)
    val k11 = M(0,0)-M(1,1)-M(2,2)
    val k12 = M(0,1)+M(1,0)    
    val k13 = M(2,0)+M(0,2)
    val k22 = -1f*M(0,0)+M(1,1)-M(2,2)
    val k23 = M(1,2)+M(2,1)  
    val k33 = -1f*M(0,0) - M(1,1) + M(2,2)   
    val K = DenseMatrix(DenseVector(k00, k01, k02, k03),
                        DenseVector(k01, k11, k12, k13),
                        DenseVector(k02, k12, k22, k23),
                        DenseVector(k03, k13, k23, k33))
    
    // coefficients of the characteristic polynomial
    val c2 = -2f*square(M).sum
    val c1 = -8f*det(M)

    // 4x4 determinant of the K matrix
    val c0 = det(K)
    
    // println("c0: " + c0)
    // println("c1: " + c1)
    // println("c2: " + c2)

    // iterate newton descent        
    val linit = (gx + gy) / 2.0
    val lambda = 
      untilconverged(linit, (cur: Rep[Double]) => abs(.000001*cur), 50, false) { lambda =>
        val l2 = lambda*lambda
        val b = (l2 + c2)*lambda
        val a = b + c1
        lambda - (a * lambda + c0) / (2.0*lambda*l2 + b + a)
      }

    // direct solve
    // val lambda = directSolve(linit, c0, c1, c2) 

    // println("lambda: " + lambda)
    val rmsd2 = (gx + gy - 2.0 * lambda) / realLen
    // println("+++STAGING MARKER: leaving perf critical")
    if (rmsd2 > 0) sqrt(rmsd2).floatValue else 0f
  }
  
  def rmsd(x: Interface[Vector[XYZ]], y: Interface[Vector[XYZ]]): Rep[Float] = {
    val xCentered = x - sum(x)/x.length
    val yCentered = y - sum(y)/y.length
    
    // compute the inner products of the individual structures
    val sx = square(xCentered).sum
    val gx = sx.x + sx.y + sx.z
    val sy = square(yCentered).sum
    val gy = sy.x + sy.y + sy.z
    
    rmsd_centered(x.length, xCentered, yCentered, gx, gy)
  }
  
  /*
   * Calculate a vector of distances from the ith frame of Theo1 to all the frames
   * of Theo2.
   */  
  def oneToAll(theo1: Rep[Theo], theo2: Rep[Theo], i: Rep[Int]): Rep[DenseVector[Float]] = {
    // -- BEGIN PERFORMANCE CRITICAL SECTION -- 
    val a = theo1.XYZData
    val b = theo2.XYZData
    val ga = theo1.G
    val gb = theo2.G
    (0::len(theo2)) { k =>
      // rmsd(a(i),b(k))
      rmsd_centered(theo1.numAtoms,a(i),b(k),ga(i),gb(k))
      // rmsd(theo1.numAtoms, theo1.numAtomsWithPadding, a(i), b(k), theo2.G(i), theo2.G(k)))
    }
    
    // STEP 1: simply call the c lib

    // nrealatoms = Theo1.NumAtoms,
    // npaddedatoms = Theo1.NumAtomsWithPadding,
    // rowstride = Theo1.NumAtomsWithPadding,
    // AData = ary_coorda = Theo2.XYZData,
    // BData = ary_coordb = Theo1.XYZData(i)
    // GAData = ary_GA = Theo2.G
    // G_y = Theo1.G(i)
     
    // for (int i = 0; i < arrayADims[0]; i++) 
    //      {
    //             float msd = ls_rmsd2_aligned_T_g(nrealatoms,npaddedatoms,rowstride,(AData+i*truestride),BData,GAData[i],G_y);
    //        Distances[i] = sqrtf(msd);
    //      }
    
    // return rmsdcalc.getMultipleRMSDs_aligned_T_g(
    //        Theo1.NumAtoms, Theo1.NumAtomsWithPadding,
    //        Theo1.NumAtomsWithPadding, Theo2.XYZData,
    //        Theo1.XYZData[index1], Theo2.G,
    //        Theo1.G[index1])
    
    // -- END PERFORMANCE CRITICAL SECTION --    
  }
  
  /*
   * Cluster the conformations in the prepared trajectory (Theo) into k clusters based on the RMSD distance
   * metric and the kcenters clustering algorithm.
   * 
   * This is a cheap (and pretty bad) clustering algorithm
   */     
  def kcenters(theo: Rep[Theo], k: Rep[Int]): (Rep[IndexVectorDense], Rep[DenseVector[Int]], Rep[DenseVector[Float]]) = {
    val numOfConformations = len(theo)
    
    var newCenter = 0 // we start by selecting (arbitrarily) the zeroth frame to be initial center
    val centerIndices = IndexVector(0,true) // indices of the identified kcenters
    
    // distanceList holds the distance of every conformation to its closest center
    val distanceList = ((0::numOfConformations) { i => INF.floatValue }).mutable
    
    // assignments holds the index of the center that each conformation is closest to
    val assignments = ((0::numOfConformations) { i => -1 }).mutable
    
    var i = 0
    while (i < k) {
      vprint("Finding Generator " + i + "\\n")
      centerIndices += newCenter
      val distanceToNewCenter = oneToAll(theo,theo,newCenter)
      val updatedIndices = (0::numOfConformations) filter { i => distanceToNewCenter(i) < distanceList(i) }
      for (ui <- updatedIndices) distanceList(ui) = distanceToNewCenter(ui) // AKS TODO: indexvector update with indexvector values
      assignments(updatedIndices) = newCenter
      
      // pick the data point that's currently worst assigned as our new center
      // INVESTIGATE: the following line is causing a violated of effect ordering error if distanceList is marked as mutable
      //newCenter = distanceList.maxIndex
      newCenter = distanceList.unsafeImmutable.maxIndex // will this be ordered correctly w.r.t. to the distanceList update?
      
      i += 1
    }  
    
    (centerIndices.unsafeImmutable, assignments.unsafeImmutable, distanceList.unsafeImmutable)       
  }
  
  /*
   * Cluster the conformations in the prepared trajectory (Theo) into k clusters based on the RMSD distance
   * metric and the CLARANS clustering algorithm.
   * 
   * http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=01033770
   *
   * Accuracy and cost are both higher as you increase the num_local_minima and max_neighbors parametors.
   * The other parameters should probably be left with their default values.
   */  
  def clarans(ptraj: Rep[Theo], 
              k: Rep[Int], 
              numLocalMinima: Rep[Int], 
              maxNeighbors: Rep[Int],
              localSwap: Rep[Boolean] = true,
              medoidsMethod: Rep[String] = "kcenters"): Rep[DenseVector[Int]] = {

    val numFrames = len(ptraj)
    
    val (initialMedoids, initialAssignments, initialDistance): (Rep[IndexVectorDense],Rep[DenseVector[Int]],Rep[DenseVector[Float]]) = t3 {
      if (medoidsMethod == "kcenters") {
        kcenters(ptraj, k)
      }
      else fatal("unsupported medoidsMethod")
    }
  
    if (initialAssignments.length != numFrames) fatal("Initial assignments is not the same length as ptraj")
    if (initialDistance.length != numFrames) fatal("Initial distance is not the same length as ptraj")
    if (initialMedoids.length != k) fatal("Initial medoids not the same length as k")
    
    val initialPMedoids = get(ptraj, initialMedoids)
    val initialCost = sum(initialDistance)
    var minCost = initialCost
    
    // stores the result
    var optimalMedoids = initialMedoids
    var optimalAssignments = initialAssignments
    var optimalDistances = initialDistance
    
    for (i <- 0::numLocalMinima) {
      vprint(i + " of " + numLocalMinima + " local minima\\n")
      
      // the canonical clarans approach is to initialize the medoids that you
      // start from randomly, but instead we use the kcenters medoids
      
      var medoids = initialMedoids
      var pMedoids = initialPMedoids
      var assignments = initialAssignments
      var distanceToCurrent = initialDistance
      var currentCost = initialCost
            
      // loop over neighbors
      var j = 0
      while (j < maxNeighbors) {
        val medoidI = random(k)
        val oldMedoid = medoids(medoidI)
        
        val trialMedoid = 
          if (!localSwap) {
            random(numFrames)
          }
          else {
            random(assignments.find(_ == medoids(medoidI)))
          }
        
        val newMedoids = medoids.mutable
        newMedoids(medoidI) = trialMedoid
        pMedoids = get(ptraj,newMedoids)
        
        val newDistances = distanceToCurrent.mutable
        val newAssignments = assignments.mutable
        
        vprint("  swapping " + oldMedoid + " for " + trialMedoid + "... ")
        
        val distanceToTrial = oneToAll(ptraj, ptraj, trialMedoid)
        val assignedToTrial = (0::distanceToTrial.length) filter { i => distanceToTrial(i) < distanceToCurrent(i) }
        // println("assignedToTrial:")
        // assignedToTrial.pprint
        newAssignments(assignedToTrial) = trialMedoid
        for (ui <- assignedToTrial) newDistances(ui) = distanceToTrial(ui)
        // println("newDistances:")
        // newDistances.pprint
        
        //val ambiguousCandidates = newAssignments.find(_ == oldMedoid)
        val ambiguous = (0::newAssignments.length) filter { i => newAssignments(i) == oldMedoid && distanceToTrial(i) >= distanceToCurrent(i) }
        // println("ambiguous:")
        // ambiguous.pprint
        for (l <- ambiguous) {
          val d = oneToAll(ptraj, pMedoids, l)
          val argmin = d.minIndex
          newAssignments(l) = newMedoids(argmin)
          newDistances(l) = d(argmin)
        }
        
        val newCost = sum(newDistances)
        if (newCost < currentCost) {
          vprint("Accept\\n")
          medoids = newMedoids.unsafeImmutable
          assignments = newAssignments.unsafeImmutable
          distanceToCurrent = newDistances.unsafeImmutable
          currentCost = newCost
          j = 0
        }
        else {
          vprint("Reject\\n")
          j += 1
        }
      }
      
      if (currentCost > minCost) {
        minCost = currentCost
        optimalMedoids = medoids.Clone
        optimalAssignments = assignments.Clone
        optimalDistances = distanceToCurrent.Clone        
      }
    }
    
    optimalMedoids    
  }
    
  def printUsage() {
    println("Usage: MiniMSM <directory containing Theo data files>")
    exit(-1)
  }
  
  // -- entry point
  def main() = {    
    if (args.length < 1) printUsage()
    val pathToTheoData = args(0).trim()
    
    if (args.length > 1) {
      if (args(1) == "+perf") {
        // test rmsd performance
        println("-- testing RMSD performance")
        //val frame = Vector[XYZ](10,true).map(e => ((randomGaussian.floatValue,randomGaussian.floatValue,randomGaussian.floatValue)))
        //val traj = Matrix[XYZ](50000,10).mapRows(e => Vector[XYZ](10,true).map(e=>((randomGaussian.floatValue,randomGaussian.floatValue,randomGaussian.floatValue))))
        val frame = readVector[XYZ](pathToTheoData + "_benchmark_frame.dat", v => ((v(0).toFloat,v(1).toFloat,v(2).toFloat)), ",")
        val traj = readMatrix[XYZ](pathToTheoData + "_benchmark_traj.dat", line => lineToXYZ(line))
        // println("frame(0): ")
        // println(frame(0).x + "," + frame(0).y + "," + frame(0).z)
        // println("traj(0,0): ")
        // println(traj(0,0).x + "," + traj(0,0).y + "," + traj(0,0).z)      
        tic()
        val out = (0::traj.numRows) { i =>
          rmsd(frame, traj(i))
        }
        toc(out)      
        println("-- test finished")
        println("out.length: " + out.length)
        out(0::10).pprint
      }
      else if (args(1) == "+perftheo") {
        println("-- testing RMSD performance with theo values")
        val frameNumAtoms = readVector(pathToTheoData + "_benchmark_frames_theo_numAtoms.dat")
        val frameTheoData = new Record {
          val XYZData = readMatrix[XYZ](pathToTheoData + "_benchmark_frames_theo_xyz.dat", line => lineToXYZ(line))
          val G = readVector[Float](pathToTheoData + "_benchmark_frames_theo_G.dat", l => l(0).toFloat)
          val numAtoms = frameNumAtoms(0).AsInstanceOf[Int]
          val numAtomsWithPadding = frameNumAtoms(1).AsInstanceOf[Int]      
        }
        val trajNumAtoms = readVector(pathToTheoData + "_benchmark_traj_theo_numAtoms.dat")
        val trajTheoData = new Record {
          val XYZData = readMatrix[XYZ](pathToTheoData + "_benchmark_traj_theo_xyz.dat", line => lineToXYZ(line))
          val G = readVector[Float](pathToTheoData + "_benchmark_traj_theo_G.dat", l => l(0).toFloat)
          val numAtoms = trajNumAtoms(0).AsInstanceOf[Int]
          val numAtomsWithPadding = trajNumAtoms(1).AsInstanceOf[Int]      
        }        
        val a = frameTheoData.XYZData
        val b = trajTheoData.XYZData    
        val ga = frameTheoData.G
        val gb = trajTheoData.G          
        tic()
        val out = (0::b.numRows) { i =>
          rmsd_centered(frameTheoData.numAtoms, a(0), b(i), ga(0), gb(i))
        }
        toc(out)      
        println("-- test finished")
        println("out.length: " + out.length)
        out(0::10).pprint                
      }
    }
    else {
      
    // option 1: load Theo data from the trajectory data created from the Python script
    // val theoData = theo(args(0))
    
    // option 2: read Theo data created from the Python script (filenames by convention)    
    val nAtoms = readVector(pathToTheoData + "_numAtoms.dat")
    val theoData = new Record {
      val XYZData = readMatrix[XYZ](pathToTheoData + "_xyz.dat", line => lineToXYZ(line))
      val G = readVector[Float](pathToTheoData + "_G.dat", l => l(0).toFloat)
      val numAtoms = nAtoms(0).AsInstanceOf[Int]
      val numAtomsWithPadding = nAtoms(1).AsInstanceOf[Int]      
    }
    
    /*
     * input testing
     */     
    /*
    println("theoData -- ")
    println("numAtoms: " + theoData.numAtoms)
    println("numAtomsWithPadding: " + theoData.numAtomsWithPadding)
    println("G.length: " + theoData.G.length)
    // -- problem with applyDynamic
    val G = theoData.G
    println("G(0): " + G(0))
    val XYZData = theoData.XYZData
    println("XYZData dims: " + theoData.XYZData.numRows + " x " + theoData.XYZData.numCols)
    println("XYZData(0,0): " + XYZData(0,0).x + "," + XYZData(0,0).y + "," + XYZData(0,0).z)
    exit(0)    
    */
    
    /*
     * run clarans clustering
     *
     * The execution time is mostly set by num_local_minima and max_neighbors
     * (lower numbers go faster). The execution time should be roughly
     * to num_local_minima, and scales worse with max_neighbors
     *
     * Also, as you scale to bigger num_local_minima and max_neighbors, the percentage
     * of the execution time spent in the C extension code goes up.
     */    
    tic()
    val numClusters = 100
    val centers = clarans(theoData, numClusters, 5, 5)
    toc(centers)
    
    println("found " + numClusters + " centers with kcenters")
    centers.pprint
    
    }
  }
  
}