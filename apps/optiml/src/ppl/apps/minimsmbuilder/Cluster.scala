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

object MiniMSMApplicationRunner extends OptiMLApplicationRunner with Clarans 
  
trait Clarans extends OptiMLApplication with TheoData {
  lazy val verbose = true
  
  def vprint(s: Rep[String]) = if (verbose) println(s)
  
  /*
   * Calculate a vector of distances from the ith frame of Theo1 to all the frames
   * of Theo2.
   */
  def oneToAll(x: Rep[Theo], y: Rep[Theo], i: Rep[Int]): Rep[DenseVector[Float]] = {

    fatal("oneToAll is TBD")
    DenseVector(0f)
    
    // -- BEGIN PERFORMANCE CRITICAL SECTION -- 
    // STEP 1: simply call the c lib
    
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
      vprint("Finding Generator " + i)
      centerIndices += newCenter
      val distanceToNewCenter = oneToAll(theo,theo,newCenter)
      val updatedIndices = (0::numOfConformations) filter { i => distanceToNewCenter(i) < distanceList(i) }
      for (ui <- updatedIndices) distanceList(ui) = distanceToNewCenter(ui)
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
      vprint(i + " of " + numLocalMinima + " local minima")
      
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
            random(assignments.filter(_ == medoids(medoidI)))
          }
        
        val newMedoids = medoids.mutable
        newMedoids(medoidI) = trialMedoid
        pMedoids = get(ptraj,newMedoids)
        
        val newDistances = distanceToCurrent.mutable
        val newAssignments = assignments.mutable
        
        vprint("  swapping " + oldMedoid + " for " + trialMedoid)
        
        val distanceToTrial = oneToAll(ptraj, ptraj, trialMedoid)
        val assignedToTrial = (0::distanceToTrial.length) filter { i => distanceToTrial(i) < distanceToCurrent(i) }
        newAssignments(assignedToTrial) = trialMedoid
        for (ui <- assignedToTrial) newDistances(ui) = distanceToTrial(ui)
        
        val ambiguousCandidates = newAssignments.filter(_ == oldMedoid)
        val ambiguous = (0::ambiguousCandidates.length) filter { i => distanceToTrial(i) >= distanceToCurrent(i) }
        for (l <- ambiguous) {
          val d = oneToAll(ptraj, pMedoids, l)
          val argmin = d.minIndex
          newAssignments(l) = newMedoids(argmin)
          newDistances(l) = d(argmin)
        }
        
        val newCost = sum(newDistances)
        if (newCost < currentCost) {
          vprint("Accept")
          medoids = newMedoids.unsafeImmutable
          assignments = newAssignments.unsafeImmutable
          distanceToCurrent = newDistances.unsafeImmutable
          currentCost = newCost
          j = 0
        }
        else {
          vprint("Reject")
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
    
    // option 1: load Theo data from the trajectory data created from the Python script
    // val theoData = theo(args(0))
    
    // option 2: read Theo data created from the Python script (filenames by convention)
    val pathToTheoData = args(0)
    val nAtoms = readVector(pathToTheoData + "_numAtoms.dat")
    val theoData = new Record {
      val XYZData = readVector[XYZ](pathToTheoData + "_xyz.dat", lineToXYZ)
      val G = readMatrix[Float](pathToTheoData + " _G.dat", str => str.toFloat)
      val numAtoms = nAtoms(0).AsInstanceOf[Int]
      val numAtomsWithPadding = nAtoms(1).AsInstanceOf[Int]      
    }
        
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
    val numClusters = 100
    val centers = clarans(theoData, numClusters, 5, 5)
    
    println("found " + numClusters + " centers with kcenters")
    centers.pprint
  }
  
}