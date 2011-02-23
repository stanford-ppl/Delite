package ppl.scalatest

import scala.virtualization.lms.common._
import ppl.delite.framework.DeliteApplication

import java.io.{ PrintWriter, FileWriter }

class TestAppCodegen extends FileDiffSuite {
  
  val prefix = "test-out/scalagen/"
  
  private def testApp(name: String, app: DeliteApplication) = {
    withOutFile(prefix+name+"-log") {
      app.generateScalaSource(new PrintWriter(new FileWriter(prefix+name+"-src")))
    }
    assertFileEqualsCheck(prefix+name+"-log")
    assertFileEqualsCheck(prefix+name+"-src")
  }

  private def testAppFusing(name: String, app: DeliteApplication) = {
    withOutFile(prefix+name+"-fusing-log") {
      val save = ppl.delite.framework.Config.opfusionEnabled
      try {
        ppl.delite.framework.Config.opfusionEnabled = true
        app.generateScalaSource(new PrintWriter(new FileWriter(prefix+name+"-fusing-src")))
      } finally {
        ppl.delite.framework.Config.opfusionEnabled = save
      }
    }
    assertFileEqualsCheck(prefix+name+"-fusing-log")
    assertFileEqualsCheck(prefix+name+"-fusing-src")
  }


  
  def testGDA = testApp("gda", ppl.apps.ml.gda.GDARunner)

  def testKMeans = testApp("kmeans", ppl.apps.ml.kmeans.kmeansRunner)

  def testLBPDenoise = testApp("lbpdenoise", ppl.apps.ml.lbpdenoise.LBPDenoiseRunner)

  def testLinReg = testApp("linreg", ppl.apps.ml.linreg.LinRegRunner)

  def testNaiveBayes = testApp("nb", ppl.apps.ml.nb.NaiveBayesRunner)

  def testRBM = testApp("rbm", ppl.apps.ml.rbm.RBMRunner)

  def testSVM = testApp("svm", ppl.apps.ml.svm.SVMRunner)

  def testSVMFusing = testAppFusing("svm", ppl.apps.ml.svm.SVMRunner)
  
  def testGradient = testApp("gradient", ppl.apps.robotics.gradient.gradientRunner)

}