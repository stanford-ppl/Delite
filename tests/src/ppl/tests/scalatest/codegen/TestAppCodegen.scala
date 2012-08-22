package ppl.tests.scalatest.codegen

import scala.virtualization.lms.common._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import ppl.delite.framework.{Config, DeliteApplication}

import java.io.{ PrintWriter, FileWriter }

class TestAppCodegen extends FileDiffSuite {
  
  val prefix = "test-out/scalagen/"
  
  private def testApp(name: String, app: DeliteApplication, args: Array[String] = Array()) = {
    withOutFile(prefix+name+"-log") {
      app.simpleCodegen = true
      app.generateScalaSource(app.getClass.getSimpleName.dropRight(1), new PrintWriter(new FileWriter(prefix+name+"-src")))
      println("##### all definitions")
      app.globalDefs.foreach { d =>
        println(d)
        val s = d match { case app.TP(sym,_) => sym; case app.TTP(syms,_,_) => syms(0); }
        val info = s.sourceInfo.drop(3).takeWhile(_.getMethodName!="main")
        println(info.map(s=>s.getFileName+":"+s.getLineNumber).distinct.mkString(","))
        //println(info.mkString(","))
      }
    }
    withOutFile(prefix+name+"-skel") {
      summarizeFile(prefix+name+"-src")
    }
/*    withOutFile(prefix+name+"-run") {
      app.setupCompiler()
      //app.compiler.settings.classpath.value = app.compiler.settings.classpath.value + ":" + Config.buildDir + "/scala"
      val generated = (new java.io.File(Config.buildDir + "/scala") listFiles) map (_.getPath)
      val compiler = app.compiler
      val reporter = app.reporter
      val run = new compiler.Run
      val fileSystem = new VirtualDirectory("<vfs>", None)
      compiler.settings.outputDirs.setSingleOutput(fileSystem)
      run.compile(List(prefix+name+"-src")++generated)
      reporter.printSummary()
      reporter.reset
      val parent = this.getClass.getClassLoader
      val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
      val cls: Class[_] = loader.loadClass("Application")
      val obj = cls.newInstance().asInstanceOf[Array[String]=>Unit]
      obj.apply(args)
    }
*/
    assert(!app.hadErrors, "Errors during code generation")
    assertFileEqualsCheckModulo(prefix+name+"-skel")("[0-9]+", "")
    //assertFileEqualsCheck(prefix+name+"-log")
    //assertFileEqualsCheck(prefix+name+"-src")
    //assertFileEqualsCheck(prefix+name+"-run")
  }

  private def testAppFusing(name: String, app: DeliteApplication, args: Array[String] = Array()) = {
    withOutFile(prefix+name+"-fusing-log") {
      val save = ppl.delite.framework.Config.opfusionEnabled
      try {
        ppl.delite.framework.Config.opfusionEnabled = true
        app.simpleCodegen = true
        app.generateScalaSource(app.getClass.getSimpleName.dropRight(1)+"Fusing",new PrintWriter(new FileWriter(prefix+name+"-fusing-src")))
        println("##### all definitions")
        app.globalDefs.foreach { d =>
          println(d)
          val s = d match { case app.TP(sym,_) => sym; case app.TTP(syms,_,_) => syms(0); }
          val info = s.sourceInfo.drop(3).takeWhile(_.getMethodName!="main")
          println(info.map(s=>s.getFileName+":"+s.getLineNumber).distinct.mkString(","))
        }
      } finally {
        ppl.delite.framework.Config.opfusionEnabled = save
      }
    }
    withOutFile(prefix+name+"-fusing-skel") {
      summarizeFile(prefix+name+"-fusing-src")
    }
    assert(!app.hadErrors, "Errors during code generation")
    assertFileEqualsCheckModulo(prefix+name+"-fusing-skel")("[0-9]+", "")
    //assertFileEqualsCheck(prefix+name+"-fusing-log")
    //assertFileEqualsCheck(prefix+name+"-fusing-src")
  }

  val datadir = "~/Desktop/tmpstuff/ppl-svn/trunk/projects/delite/data" // TODO: config

  // --- bio
  
  def testSpade = testApp("spade", ppl.apps.bio.spade.SpadeRunner)

  def testSpadeFusing = testAppFusing("spade", ppl.apps.bio.spade.SpadeRunner)
  
  // --- ml
  
  def testGDA = testApp("gda", ppl.apps.ml.gda.GDARunner, Array(datadir+"/ml/gda/q1x.dat",datadir+"/ml/gda/q1y.dat"))

  def testGDAFusing = testAppFusing("gda", ppl.apps.ml.gda.GDARunner, Array(datadir+"/ml/gda/q1x.dat",datadir+"/ml/gda/q1y.dat"))

  def testKMeans = testApp("kmeans", ppl.apps.ml.kmeans.kmeansRunner, Array(datadir+"/ml/kmeans/mandrill-large.dat",datadir+"/ml/kmeans/initmu.dat"))

  def testKMeansFusing = testAppFusing("kmeans", ppl.apps.ml.kmeans.kmeansRunner, Array(datadir+"/ml/kmeans/mandrill-large.dat",datadir+"/ml/kmeans/initmu.dat"))

  //def testLBPDenoise = testApp("lbpdenoise", ppl.apps.ml.lbpdenoise.LBPDenoiseRunner, Array(datadir+"/ml/lbp/onlyedges1",datadir+"/ml/lbp/graphprint1")) PENDING

  //def testLBPDenoiseFusing = testAppFusing("lbpdenoise", ppl.apps.ml.lbpdenoise.LBPDenoiseRunner, Array(datadir+"/ml/lbp/onlyedges1",datadir+"/ml/lbp/graphprint1"))

  def testLinReg = testApp("linreg", ppl.apps.ml.linreg.LinRegRunner, Array(datadir+"/ml/linreg/x-1024.dat",datadir+"/ml/linreg/y-1024.dat"))

  def testLinRegFusing = testAppFusing("linreg", ppl.apps.ml.linreg.LinRegRunner, Array(datadir+"/ml/linreg/x-1024.dat",datadir+"/ml/linreg/y-1024.dat"))

  def testNaiveBayes = testApp("nb", ppl.apps.ml.nb.NaiveBayesRunner, Array(datadir+"/ml/nb/MATRIX.TRAIN.25k",datadir+"/ml/nb/MATRIX.TEST"))

  def testNaiveBayesFusing = testAppFusing("nb", ppl.apps.ml.nb.NaiveBayesRunner, Array(datadir+"/ml/nb/MATRIX.TRAIN.25k",datadir+"/ml/nb/MATRIX.TEST"))

  def testRBM = testApp("rbm", ppl.apps.ml.rbm.RBMRunner, Array(datadir+"/ml/rbm/mnist2000x10.dat","2000","2000"))

  def testRBMFusing = testAppFusing("rbm", ppl.apps.ml.rbm.RBMRunner, Array(datadir+"/ml/rbm/mnist2000x10.dat","2000","2000"))

  def testSVM = testApp("svm", ppl.apps.ml.svm.SVMRunner, Array(datadir+"/ml/svm/MATRIX.TRAIN.800",datadir+"/ml/svm/MATRIX.TEST","output","10"))

  def testSVMFusing = testAppFusing("svm", ppl.apps.ml.svm.SVMRunner, Array(datadir+"/ml/svm/MATRIX.TRAIN.800",datadir+"/ml/svm/MATRIX.TEST","output","10"))

  // --- robotics
  
  //def testGradient = testApp("gradient", ppl.apps.robotics.gradient.gradientRunner) PENDING

  //def testGradientFusing = testAppFusing("gradient", ppl.apps.robotics.gradient.gradientRunner)

}