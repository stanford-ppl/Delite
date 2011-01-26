package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,GrayscaleImage}
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientGrid {
  // TODO: how do we clean this up in app code?
  val IR: DeliteApplication with OptiMLExp
  import IR._

  val BinarizedGradientPyramidFuncs = new BinarizedGradientPyramid { val IR: DeliteApplication with OptiMLExp = this.IR }
  val BinarizedGradientTemplateFuncs = new BinarizedGradientTemplate { val IR: DeliteApplication with OptiMLExp = this.IR }

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
  def detectAllObjects(all_templates: Rep[Vector[(String, Vector[BinarizedGradientTemplate])]], image: Rep[GrayscaleImage]) = {
    val img_gray = image // assuming image is single-channel. Needs to be made such if not.

    val (mag, phase) = t2(repGrayscaleImageToGrayscaleImageOps(img_gray).gradients(true))
    val binGrad = binarizeGradients(mag, phase)
    val cleanGrad = gradMorphology(binGrad)

    val pyr = ppl.dsl.optiml.datastruct.scala.BinarizedGradientPyramid(cleanGrad)

    val all_detections = aggregate(all_templates) { t =>
      val (name, templates) = t
      println(name + ": Using " + templates.length + " templates")
      val detections = detectSingleObject(name, pyr.getIndex(pyr.fixedLevelIndex), templates, template_radius_, pyr.fixedLevelIndex, accept_threshold_)
      println(name + ": Detections: " + detections.length)
      detections
    }
    val filteredDetections = nonMaxSuppress(all_detections, fraction_overlap_)
    println("Total detections: " + filteredDetections.length)
  }

  //Run detection for this object class.
  def detectSingleObject(name: Rep[String], gradSummary: Rep[GrayscaleImage], templates: Rep[Vector[BinarizedGradientTemplate]], template_radius: Rep[Int], level: Rep[Int], accept_threshold: Rep[Float]): Rep[Vector[BiGGDetection]] = {
    aggregate(borderPixels, gradSummary.numRows - borderPixels, borderPixels, gradSummary.numCols - borderPixels) {
      (x, y) => searchTemplates(name, gradSummary, x, y, template_radius, level, accept_threshold, templates)
    }
  }

  def searchTemplates(name: Rep[String], gradSummary: Rep[GrayscaleImage], x: Rep[Int], y: Rep[Int], template_radius: Rep[Int], level: Rep[Int], accept_threshold: Rep[Float], templates: Rep[Vector[BinarizedGradientTemplate]]): Rep[Vector[BiGGDetection]] = {
    val reduction_factor = (1 << level)
    val crt_template = fillTemplateFromGradientImage(gradSummary, x, y, template_radius, level)
    aggregate(0, templates.length) { j =>
      val res = BinarizedGradientTemplate.score(templates(j), crt_template, accept_threshold)
      if (res > accept_threshold) {
        val bbox = templates(j).rect
        val roi = new Rect((reduction_factor * x - bbox.width / 2).asInstanceOfL[Int], (reduction_factor * y - bbox.height / 2).asInstanceOfL[Int], bbox.width, bbox.height)
        Vector[BiGGDetection](new BiGGDetection(name, res, roi, null, j, x, y, templates(j), crt_template))
      }
      else {
        Vector[BiGGDetection]()
      }
    }
  }

  // Construct a template from a region of a gradient summary image.
  def fillTemplateFromGradientImage(gradSummary: Rep[GrayscaleImage], xc: Rep[Int], yc: Rep[Int], r: Rep[Int], level: Rep[Int]): Rep[BinarizedGradientTemplate] = {
    val span = 2 * r
    val tpl = BinarizedGradientTemplate(r, null, null, level, Vector[Int](span * span), IndexVector(), null, null, null)

    //Bear with me, we have to worry a bit about stepping off the image boundaries:
    val (xstart, xoffset) = t2(if (xc - r < 0) (0, r - xc) else (xc - r, 0))
    val xend = if (xc + r > gradSummary.numCols) gradSummary.numCols else xc + r
    val (ystart, yoffset) = t2(if (yc - r < 0) (0, r - yc) else (yc - r, 0))
    val yend = if (yc + r > gradSummary.numRows) gradSummary.numRows else yc + r

    //Fill the binary gradients
    var y = ystart
    while (y < yend) {
      val imageRow = gradSummary.getRow(y)
      var x = xstart
      while (x < xend) {
        var index = (yoffset + y - ystart) * span + (xoffset + x - xstart) //If this were an image patch, this is the offset to it
        tpl.binary_gradients(index) = imageRow(x)
        if (imageRow(x) > 0) {
          //Record where gradients are
          tpl.match_list += index
        }
        x += 1
      }
      y += 1
    }
    tpl
  }

  //Turn mag and phase into a binary representation of 8 gradient directions.
  def binarizeGradients(mag: Rep[Matrix[Float]], phase: Rep[Matrix[Float]]): Rep[GrayscaleImage] = {
    GrayscaleImage(mag.zipWith(phase, (a, b) => {
      if (a >= magnitude_threshold_) {
          var angle = b
          if (angle >= 180) {
            angle -= 180 //Ignore polarity of the angle
          }
          (1 << (angle / (180.0 / 8)).asInstanceOfL[Int])
        }
      else 0
    }))
  }

  // Filter out noisy gradients via non-max suppression in a 3x3 area.
  def gradMorphology(binaryGradient: Rep[GrayscaleImage]): Rep[GrayscaleImage] = {
    //Zero the borders -- they are unreliable
    binaryGradient.getRow(0) = 0
    binaryGradient.getRow(binaryGradient.numRows - 1) = 0
    binaryGradient.getCol(0) = 0
    binaryGradient.getCol(binaryGradient.numCols - 1) = 0

    // non-max suppression over a 3x3 stencil throughout the entire binaryGradient image
    // (Each pixel location contains just one orientation at this point)
    binaryGradient.windowedFilter (3, 3) { slice /*3x3 Matrix[T]*/ =>
      // for each element, pick the most frequently occurring gradient direction if it's at least 2; otherwise pick 0(no direction)
      val histogram = Vector[Int](255)
      // TODO: Make this a scan-like op once supported
      var row = 0
      while (row < slice.numRows) {
        var col = 0
        while (col < slice.numCols) {
          histogram(slice(row, col)) += 1
          col += 1
        }
        row += 1
      }
      if (histogram.max >= 2) histogram.maxIndex else 0
    }
  }

  // Determines if two rectangles intersect (true = intersects)
  def intersect(a: Rep[Rect], b: Rep[Rect]): Rep[Boolean] = {
    ((a.x < (b.x + b.width)) && ((a.x + a.width) > b.x) && ((a.y + a.height) > b.y) && (a.y < (b.y + b.height)))
  }

  // Computes the fraction of the intersection of two rectangles with respect to the total area of the rectangles.
  def rectFractOverlap(a: Rep[Rect], b: Rep[Rect]): Rep[Float] = {
    if (intersect(a, b)) {
      val total_area: Float = b.height * b.width + a.width * a.height
      val left = if (a.x > b.x) a.x else b.x
      val top = if (a.y > b.y) a.y else b.y
      val right = if (a.x + a.width < b.x + b.width) a.x + a.width else b.x + b.width
      val width = right - left
      val bottom = if (a.y + a.height < b.y + b.height) a.y + a.height else b.y + b.height
      val height = bottom - top
      2.0f * height * width / (total_area + 0.000001f) //Return the fraction of intersection
    } else {
      0.0f
    }
  }

  // Suppress overlapping rectangles to be the rectangle with the highest score
  // detections: vector of detections to work with
  // overlapThreshold: what fraction of overlap between 2 rectangles constitutes overlap
  def nonMaxSuppress(detections: Rep[Vector[BiGGDetection]], overlapThreshold: Rep[Float]): Rep[Vector[BiGGDetection]] = {
    var len = detections.length

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
            len -= 1
            j -= 1
          }
          else {
            val temp = detections(len - 1)
            detections(len - 1) = detections(i)
            detections(i) = temp
            len -= 1
            i -= 1
            iMoved = true
          }
        }
        j += 1
      }
      i += 1
    }
    detections.take(len)
  }
}