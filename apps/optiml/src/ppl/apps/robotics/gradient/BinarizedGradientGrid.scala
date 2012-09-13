package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.application._
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientGridFuncs {
  this: OptiMLApplication with BinarizedGradientPyramidFuncs with BinarizedGradientTemplateFuncs =>

  // The radius of the template
  val template_radius_ = 15

  // Threshold for accepting a template match (1 is perfect)
  val accept_threshold_ = 0.82f

  // Ignore gradients with lower magnitude
  val magnitude_threshold_ = 200

  // Fraction overlap between two detections above which one of them is suppressed
  val fraction_overlap_ = 0.6f

  val borderPixels = 5

  // Runs the object detection of the current image.
  def detectAllObjects(all_templates: Rep[DenseVector[(String, DenseVector[BinarizedGradientTemplate])]], image: Rep[GrayscaleImage]) = {
    val img_gray = image // assuming image is single-channel. Needs to be made such if not.

    val (mag: Rep[DenseMatrix[Double]], phase: Rep[DenseMatrix[Double]]) = t2(img_gray.gradients(true))
    val binGrad = binarizeGradients(mag, phase)  
    val cleanGrad = gradMorphology(binGrad)
    val pyr = makePyramid(cleanGrad)    
    val all_detections = all_templates.flatMap { t => 
      val (name, templates) = t2(t)
      println("Name: " + name)
      println("Templates: " + templates.length)
      val detections = detectSingleObject(name, getIndex(pyr, pyr.fixedLevelIndex), templates, template_radius_, pyr.fixedLevelIndex, accept_threshold_)
      println("Detections: " + detections.length)
      detections
    }
    //println("Detections before NMS: " + all_detections.length)
    val filteredDetections = nonMaxSuppress(all_detections, fraction_overlap_)
    println("Total detections: " + filteredDetections.length)
  }

  // Run detection for this object class.
  def detectSingleObject(name: Rep[String], gradSummary: Rep[GrayscaleImage], templates: Rep[DenseVector[BinarizedGradientTemplate]], template_radius: Rep[Int], level: Rep[Int], accept_threshold: Rep[Float]): Rep[DenseVector[BiGGDetection]] = {
    (borderPixels :: gradSummary.numRows - borderPixels).flatMap { y =>
      (borderPixels :: gradSummary.numCols - borderPixels).flatMap { x =>
        searchTemplates(name, gradSummary, x, y, template_radius, level, accept_threshold, templates)
      }
    }
  }

  def searchTemplates(name: Rep[String], gradSummary: Rep[GrayscaleImage], x: Rep[Int], y: Rep[Int], template_radius: Rep[Int], level: Rep[Int], accept_threshold: Rep[Float], templates: Rep[DenseVector[BinarizedGradientTemplate]]): Rep[DenseVector[BiGGDetection]] = {
    // println("x: " + x)
    // println("y: " + y)
    val reduction_factor = pow(2, level).AsInstanceOf[Int]  //(1 << level)
    //println("reduction_factor: " + reduction_factor)
    val crt_template = fillTemplateFromGradientImage(gradSummary, x, y, template_radius, level)
    // println("crt_template match_list length: " + crt_template.match_list.length)
    if (crt_template.match_list.length < 0) println("dummy")
    aggregateIf(0,templates.length)(j => score(templates(j), crt_template, accept_threshold) > accept_threshold) { j =>
      val res = score(templates(j), crt_template, accept_threshold)
      val bbox = templates(j).rect
      val roi = Rect((reduction_factor * x - bbox.width / 2).AsInstanceOf[Int], (reduction_factor * y - bbox.height / 2).AsInstanceOf[Int], bbox.width, bbox.height)
      BiGGDetection(name, res, roi, null, j, x, y, templates(j), crt_template)
    }
  }

  // Construct a template from a region of a gradient summary image.
  def fillTemplateFromGradientImage(gradSummary: Rep[GrayscaleImage], xc: Rep[Int], yc: Rep[Int], r: Rep[Int], level: Rep[Int]): Rep[BinarizedGradientTemplate] = {
    val span = 2 * r

    // Bear with me, we have to worry a bit about stepping off the image boundaries:
    val (xstart, xoffset) = t2 { if (xc - r < 0) (unit(0), r - xc) else (xc - r, unit(0)) }
    val xend = if (xc + r > gradSummary.numCols) gradSummary.numCols else xc + r
    val (ystart, yoffset) = t2 { if (yc - r < 0) (unit(0), r - yc) else (yc - r, unit(0)) }
    val yend = if (yc + r > gradSummary.numRows) gradSummary.numRows else yc + r

    val binaryGradients = DenseVector[Double](span*span, false)
    val matchList = IndexVector(0, true)
    
    // Fill the binary gradients
    var y = ystart
    while (y < yend) {
      val imageRow = gradSummary(y)
      var x = xstart
      while (x < xend) {
        val index = (yoffset + y - ystart) * span + (xoffset + x - xstart) // If this were an image patch, this is the offset to it
        binaryGradients(index) = imageRow(x) 
        if (imageRow(x) > 0) {
          // Record where gradients are
          matchList <<= index
        }
        x += 1
      }
      y += 1
    }
    //println(matchList.length)
    BinarizedGradientTemplate(r, null, null, level, binaryGradients, matchList, null, null, null)    
  }

  // Turn mag and phase into a binary representation of 8 gradient directions.
  def binarizeGradients(mag: Rep[DenseMatrix[Double]], phase: Rep[DenseMatrix[Double]]): Rep[GrayscaleImage] = {
    GrayscaleImage((mag zip phase) {(a,b) => {
      if (a >= magnitude_threshold_) {
          var angle = b
          if (angle >= 180) {
            angle = angle - 180 // Ignore polarity of the angle
          }
          pow(2, (angle.AsInstanceOf[Double] / (180.0 / 8)).AsInstanceOf[Int])
        }
      else 0
    }})
  }

  // Filter out noisy gradients via non-max suppression in a 3x3 area.
  def gradMorphology(binaryGradientWithBorders: Rep[GrayscaleImage]): Rep[GrayscaleImage] = {
    // TODO: what is the proper way to do this kind of in-place zeroing?
    val binaryGradient = GrayscaleImage(binaryGradientWithBorders) //binaryGradientWithBorders.mutable
    
    // Zero the borders -- they are unreliable
    binaryGradient.getRow(0).unsafeMutable.mmap { e => 0 } 
    binaryGradient.getRow(binaryGradient.numRows - 1).unsafeMutable.mmap { e => 0 }  
    binaryGradient.getCol(0).unsafeMutable.mmap { e => 0 }  
    binaryGradient.getCol(binaryGradient.numCols - 1).unsafeMutable.mmap { e => 0 }  

    // non-max suppression over a 3x3 stencil throughout the entire binaryGradient image
    // (Each pixel location contains just one orientation at this point)
    binaryGradient.windowedFilter (3, 3) { slice /*3x3 GrayscaleImage*/ =>
      // for each element, pick the most frequently occurring gradient direction if it's at least 2; otherwise pick 0 (no direction)
      val histogram = slice.histogram
      if (histogram.max > 1) histogram.maxIndex else 0      
    }
  }

  // Determines if two rectangles intersect (true = intersects)
  def intersect(a: Rep[Rect], b: Rep[Rect]): Rep[Boolean] = {
    ((a.x < (b.x + b.width)) && ((a.x + a.width) > b.x) && ((a.y + a.height) > b.y) && (a.y < (b.y + b.height)))
  }

  // Computes the fraction of the intersection of two rectangles with respect to the total area of the rectangles.
  def rectFractOverlap(a: Rep[Rect], b: Rep[Rect]): Rep[Float] = {
    if (intersect(a, b)) {
      val total_area: Rep[Float] = b.height * b.width + a.width * a.height
      val left = if (a.x > b.x) a.x else b.x // These conditionals do not generate properly
      val top = if (a.y > b.y) a.y else b.y
      val right = if (a.x + a.width < b.x + b.width) a.x + a.width else b.x + b.width
      val width = right - left
      val bottom = if (a.y + a.height < b.y + b.height) a.y + a.height else b.y + b.height
      val height = bottom - top
      2.0f * height * width / (total_area + 0.000001f) // Return the fraction of intersection
    } else {
      0.0f
    }
  }

  // Suppress overlapping rectangles to be the rectangle with the highest score
  // detections: vector of detections to work with
  // overlapThreshold: what fraction of overlap between 2 rectangles constitutes overlap
  def nonMaxSuppress(detections: Rep[DenseVector[BiGGDetection]], overlapThreshold: Rep[Float]): Rep[DenseVector[BiGGDetection]] = {
    var len = detections.length
    println("Detections before NMS: " + len)

//    detections filter { d1 =>
//      var isMax = true
//      var d2Index = 0
//      while (d2Index < detections.length) {
//        val d2 = detections(d2Index)
//        if (d1 != d2) {
//          val measuredOverlap = rectFractOverlap(d1.roi, d2.roi)
//          if (measuredOverlap > overlapThreshold) {
//            if (d1.score < d2.score) {
//              isMax = false
//            }
//          }
//        }
//        d2Index += 1
//      }
//      isMax
//    }


    // TODO: we can likely rewrite this to filter not in place, avoiding this copy
    val detectionsOut = detections.mutable
    
    var i = 0
    while (i < len - 1) {
      var j = i + 1
      var iMoved = false
      while (j < len && iMoved == false) {
        val measured_frac_overlap = rectFractOverlap(detectionsOut(i).roi, detectionsOut(j).roi)
        //println("overlap: " + measured_frac_overlap)
        if (measured_frac_overlap > overlapThreshold) {
          //println("score i: " + detections(i).score)
          //println("score j: " + detections(j).score)
          if (detectionsOut(i).score >= detectionsOut(j).score) {
            val temp = detectionsOut(len - 1)
            detectionsOut(len - 1) = detectionsOut(j) 
            detectionsOut(j) = temp 
            len = len - 1
            j = j - 1
          }
          else {
            val temp = detectionsOut(len - 1)
            detectionsOut(len - 1) = detectionsOut(i) 
            detectionsOut(i) = temp 
            len = len - 1
            i = i - 1
            iMoved = true
          }
        }
        j += 1
      }
      i += 1
    }
// }
    detectionsOut.take(len)
  }

/*
  // Suppress overlapping rectangles to be the rectangle with the highest score
  // detections: vector of detections to work with
  // overlapThreshold: what fraction of overlap between 2 rectangles constitutes overlap
  def nonMaxSuppressAlternativeImpl(detections: Rep[Vector[BiGGDetection]], overlapThreshold: Rep[Float]): Rep[Vector[BiGGDetection]] = {
    var len = detections.length

    var i = 0
    while (i < len - 1) {
      var j = i + 1
      var iMoved = false
      while (j < len && iMoved == false) {
        val measured_frac_overlap = rectFractOverlap(detections(i).roi, detections(j).roi)
        if (measured_frac_overlap > overlapThreshold) {
          if (detections(i).score >= detections(j).score) {
            val temp = detections(len - 1)
            detections(len - 1) = detections(j)
            detections(j) = temp
            len = len - 1
            j = j - 1
          }
          else {
            val temp = detections(len - 1)
            detections(len - 1) = detections(i)
            detections(i) = temp
            len = len - 1
            i = i - 1
            iMoved = true
          }
        }
        j += 1
      }
      i += 1
    }
    detections.take(len)


//    detections filter { d1 =>
//      var isMax = true
//      var d2Index = 0
//      while (d2Index < detections.length) {
//        val d2 = detections(d2Index)
//        if (d1 != d2) {
//          val measuredOverlap = rectFractOverlap(d1.roi, d2.roi)
//          if (measuredOverlap > overlapThreshold) {
//            if (d1.score < d2.score) {
//              isMax = false
//            }
//          }
//        }
//        d2Index += 1
//      }
//      isMax
//    }


    val output = Vector[BiGGDetection](0, true)

    var i = 0
    while (i < len - 1) {
      var j = i + 1
      var iMoved = false
      while (j < len) {
        if (rectFractOverlap(detections(i).roi, detections(j).roi) > overlapThreshold) {
          output <<= if (detections(i).score >= detections(j).score) detections(i) else detections(j)
        }
        j += 1
      }
      i += 1
    }
    output
  }
*/
}
