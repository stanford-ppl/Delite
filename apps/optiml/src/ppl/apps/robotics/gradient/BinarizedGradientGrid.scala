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
//    println("detectAllObjects.1")
    val img_gray = image // assuming image is single-channel. Needs to be made such if not.

//    println("detectAllObjects.2")

    val (mag: Rep[DenseMatrix[Float]], phase: Rep[DenseMatrix[Float]]) = t2(repGrayscaleImageToGrayscaleImageOps(img_gray).gradients(true))
//    println("detectAllObjects.3")
    val binGrad = binarizeGradients(mag, phase)
//    println("detectAllObjects.4")
    val cleanGrad = gradMorphology(binGrad)
//    println("detectAllObjects.5")

    val pyr = makePyramid(cleanGrad)
//    println("detectAllObjects.6")

    val all_detections = all_templates.flatMap { t => //TODO TR written to in nonMaxSuppress
    //println("detectAllObjects.7")
      val (name, templates) = t2(t)
    //println("detectAllObjects.8")
      println("Name: " + name)
      println("Templates: " + templates.length)
      val detections = detectSingleObject(name, getIndex(pyr, pyr.fixedLevelIndex), templates, template_radius_, pyr.fixedLevelIndex, accept_threshold_)
      println("Detections: " + detections.length)
      detections
    }
//    println("Detections before NMS: " + all_detections.length)
    val filteredDetections = nonMaxSuppress(all_detections, fraction_overlap_)
    println("Total detections: " + filteredDetections.length)
  }

  //Run detection for this object class.
  def detectSingleObject(name: Rep[String], gradSummary: Rep[GrayscaleImage], templates: Rep[DenseVector[BinarizedGradientTemplate]], template_radius: Rep[Int], level: Rep[Int], accept_threshold: Rep[Float]): Rep[DenseVector[BiGGDetection]] = {
//println("detectSingleObject.1")
    (borderPixels :: gradSummary.numRows - borderPixels).flatMap { y =>
//println("detectSingleObject.2")
      (borderPixels :: gradSummary.numCols - borderPixels).flatMap { x =>
//println("detectSingleObject.3")
        searchTemplates(name, gradSummary, x, y, template_radius, level, accept_threshold, templates)
      }
    }
  }

  def searchTemplates(name: Rep[String], gradSummary: Rep[GrayscaleImage], x: Rep[Int], y: Rep[Int], template_radius: Rep[Int], level: Rep[Int], accept_threshold: Rep[Float], templates: Rep[DenseVector[BinarizedGradientTemplate]]): Rep[DenseVector[BiGGDetection]] = {
//println("x: " + x)
//println("y: " + y)
//println("searchTemplates.1")
    val reduction_factor = pow(2, level).AsInstanceOf[Int]//(1 << level)
//println("searchTemplates.2")
//println("reduction_factor: " + reduction_factor)
    val crt_template = fillTemplateFromGradientImage(gradSummary, x, y, template_radius, level)
//println(crt_template.match_list.length)
if (crt_template.match_list.length < 0) println("dummy")
//println("searchTemplates.3")
    (0 :: templates.length).flatMap { j =>
      val res = score(templates(j), crt_template, accept_threshold)
      if (res > accept_threshold) {
//println("res: " + res)
//println("x: " + x)
//println("y: " + y)
        val bbox = templates(j).rect
        val roi = Rect((reduction_factor * x - bbox.width / 2).AsInstanceOf[Int], (reduction_factor * y - bbox.height / 2).AsInstanceOf[Int], bbox.width, bbox.height)
        val out = DenseVector[BiGGDetection](1, true)
        out(0) = BiGGDetection(name, res, roi, null, j, x, y, templates(j), crt_template)
        out
      }
      else {
        DenseVector[BiGGDetection](0, true)
      }
    }
  }

  // Construct a template from a region of a gradient summary image.
  def fillTemplateFromGradientImage(gradSummary: Rep[GrayscaleImage], xc: Rep[Int], yc: Rep[Int], r: Rep[Int], level: Rep[Int]): Rep[BinarizedGradientTemplate] = {
    val span = 2 * r
    val tpl = BinarizedGradientTemplate(r, null, null, level, DenseVector[Int](span * span, false), IndexVector(0), null, null, null)

    //Bear with me, we have to worry a bit about stepping off the image boundaries:
    val (xstart, xoffset) = t2(if (xc - r < 0) (unit(0), r - xc) else (xc - r, unit(0)))
    val xend = if (xc + r > gradSummary.numCols) gradSummary.numCols else xc + r
    val (ystart, yoffset) = t2(if (yc - r < 0) (unit(0), r - yc) else (yc - r, unit(0)))
    val yend = if (yc + r > gradSummary.numRows) gradSummary.numRows else yc + r

    //Fill the binary gradients
    var y = ystart
//println("fillTemplate.1")
    while (y < yend) {
      val imageRow = gradSummary.getRow(y)
      var x = xstart
      while (x < xend) {
        var index = (yoffset + y - ystart) * span + (xoffset + x - xstart) //If this were an image patch, this is the offset to it
        tpl.binary_gradients(index) = imageRow(x) //TODO TR non-mutable write
        if (imageRow(x) > 0) {
          //Record where gradients are
          tpl.match_list += index //TODO TR non-mutable write
        }
        x += 1
      }
      y += 1
    }
//println(tpl.match_list.length)
    tpl
  }

  //Turn mag and phase into a binary representation of 8 gradient directions.
  def binarizeGradients(mag: Rep[DenseMatrix[Float]], phase: Rep[DenseMatrix[Float]]): Rep[GrayscaleImage] = {
    GrayscaleImage((mag zip phase) {(a,b) => {
      if (a >= magnitude_threshold_) {
          var angle = b
          if (angle >= 180) {
            angle = angle - 180 //Ignore polarity of the angle
          }
          pow(2, (angle.AsInstanceOf[Double] / (180.0 / 8)).AsInstanceOf[Int]).AsInstanceOf[Int]
        }
      else unit(0)
    }})
  }

  // Filter out noisy gradients via non-max suppression in a 3x3 area.
  def gradMorphology(binaryGradient: Rep[GrayscaleImage]): Rep[GrayscaleImage] = {
    //Zero the borders -- they are unreliable
//    binaryGradient.getRow(0) = 0
//    binaryGradient.getRow(binaryGradient.numRows - 1) = 0
//    binaryGradient.getCol(0) = 0
//    binaryGradient.getCol(binaryGradient.numCols - 1) = 0
//    for (x <- 0 until cols) {
//      binaryGradient.data(0, x) = 0
//      binaryGradient.data(rows - 1, x) = 0
//    }
//    for (y <- 1 until rows - 1) {
//      binaryGradient.data(y, 0) = 0
//      binaryGradient.data(y, cols - 1) = 0
//    }
    binaryGradient.getRow(0).mmap {e => 0} //TODO TR non-mutable write
    binaryGradient.getRow(binaryGradient.numRows - 1).mmap {e => 0}  //TODO TR non-mutable write
    binaryGradient.getCol(0).mmap {e => 0}  //TODO TR non-mutable write
    binaryGradient.getCol(binaryGradient.numCols - 1).mmap {e => 0}  //TODO TR non-mutable write

    // non-max suppression over a 3x3 stencil throughout the entire binaryGradient image
    // (Each pixel location contains just one orientation at this point)
    repGrayscaleImageToGrayscaleImageOps(binaryGradient).windowedFilter (3, 3) { slice /*3x3 Matrix[T]*/ =>
      // for each element, pick the most frequently occurring gradient direction if it's at least 2; otherwise pick 0(no direction)
      val histogram = DenseVector[Int](256, true)
      // TODO: Make this a scan-like op once supported
      var row = 0
      while (row < slice.numRows) {
        var col = 0
        while (col < slice.numCols) {
          //histogram(slice(row, col)) += 1
          histogram(slice(row,col)) = histogram(slice(row,col))+1
          col += 1
        }
        row += 1
      }
      var i = 2
      var max = histogram(1)
      var maxIndex = 1
      while (i < histogram.length) {
        if (histogram(i) > max) {
          max = histogram(i)
          maxIndex = i
        }
        i += 1
      }
      if (max > 1) maxIndex else unit(0)
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
      // TODO: why won't this work implicitly?
      arithToArithOps(2.0f) * height * width / (total_area + 0.000001f) //Return the fraction of intersection
      // TODO: can't get this one to kick in either :(
      //chainRepArithToArithOps[Int,Float](height * width) * 2.0f / (total_area + 0.000001f) //Return the fraction of intersection
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


for (i <- 0 until 1) { //TR ?
    var i = 0
//println("nms.1")
    while (i < len - 1) {
      var j = i + 1
      var iMoved = false
//println("nms.2")
      while (j < len && iMoved == false) {
//println("nms.3")
//println("I: " + i)
//println("J: " + j)
        val measured_frac_overlap = rectFractOverlap(detections(i).roi, detections(j).roi)
//println("nms.4")
//println("overlap: " + measured_frac_overlap)
        if (measured_frac_overlap > overlapThreshold) {
//println("nms.5")
//println("score i: " + detections(i).score)
//println("score j: " + detections(j).score)
          if (detections(i).score >= detections(j).score) {
//println("nms.6")
            val temp = detections(len - 1)
            detections(len - 1) = detections(j) //TODO TR non-mutable write
            detections(j) = temp //TODO TR non-mutable write
            len = len - 1
            j = j - 1
          }
          else {
//println("nms.7")
            val temp = detections(len - 1)
            detections(len - 1) = detections(i) //TODO TR non-mutable write
            detections(i) = temp //TODO TR non-mutable write
            len = len - 1
            i = i - 1
            iMoved = true
          }
        }
        j += 1
      }
      i += 1
    }
//println("nms.8")
}
    detections.take(len)
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
          output += if (detections(i).score >= detections(j).score) detections(i) else detections(j)
        }
        j += 1
      }
      i += 1
    }
    output
  }
*/
}
