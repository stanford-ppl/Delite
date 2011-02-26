package ppl.dsl.optiql.baseline.apps


/*
import java.awt.image.{BufferedImage}
import java.awt.{Dimension, Image, Graphics, Canvas}
import javax.swing.{JFrame}
import QueryUtils._

object RayTracer {

  case class Vector(val x:Double, val y:Double, val z:Double) {

    def times(n:Double) = Vector(n * x, n * y, n * z)
    def -(v2:Vector) = Vector(x - v2.x, y - v2.y, z - v2.z)
    def +(v2:Vector) = Vector(x + v2.x, y + v2.y, z + v2.z)
    def dot(v2:Vector) = x * v2.x + y * v2.y + z * v2.z
    def cross(v2:Vector) = Vector(y * v2.z - z * v2.y, z * v2.x - x * v2.z, x * v2.y - y * v2.x)
    def mag() = Math.sqrt(this dot this)
    def norm() = {
      val magnitude = mag()
      if (magnitude == 0) times(Math.POS_INF_DOUBLE)
      else times(1 / magnitude)
    }
  }

  case class Colour(val r:Double, val g:Double, val b:Double) {

    def times(n:Double) = Colour(n * r, n * g, n * b)
    def times(c:Colour) = Colour(r * c.r, g * c.g, b * c.b)
    def plus(c:Colour) = Colour(r + c.r, g + c.g, b + c.b)
    def minus(c:Colour) = Colour(r - c.r, g - c.g, b - c.b)
    def legalize():Int = {
      val red = if (r > 1) 255 else (r * 255).asInstanceOf[Int]
      val green = if (g > 1) 255 else (g * 255).asInstanceOf[Int]
      val blue = if (b > 1) 255 else (b * 255).asInstanceOf[Int]
      (red << 16) + (green << 8) + blue
    }
  }

  object Colour {
    def background = Colour(0, 0, 0)
    def defaultColour = Colour(0, 0, 0)
  }

  abstract class SceneObject {
    val surface:Surface
    def intersect(r:Ray):ISect
    def normal(pos:Vector):Vector
  }

  case class Ray(val src:Vector, val dir:Vector)

  case class ISect(val obj:SceneObject, val ray:Ray, val dist:Double)

  case class Surface(val diff: (Vector) => Colour, val spec: (Vector) => Colour, val refl: (Vector) => Double,
                     val roughness:Double)

  object Surface {

    def checkerboard = {
      val diff = (pos:Vector) => {
        if ((Math.floor(pos.z) + Math.floor(pos.x)) % 2 != 0) Colour(1, 1, 1)
        else Colour(0, 0, 0)
      }
      val refl = (pos:Vector) => {
        if ((Math.floor(pos.z) + Math.floor(pos.x)) % 2 != 0) .1 else .7
      }
      Surface(diff, (pos:Vector) => Colour(1, 1, 1), refl, 150)
    }

    def shiny = Surface((pos:Vector) => Colour(1, 1, 1), (pos:Vector) => Colour(.5, .5, .5), (pos:Vector) => .6, 50)
  }

  case class Camera(val pos:Vector, val forward:Vector, val up:Vector, val right:Vector)

  object Camera {
    def create(pos:Vector, lookAt:Vector) = {
      val forward = (lookAt - pos).norm()
      val down = Vector(0, -1, 0)
      val right = (forward cross down).norm().times(1.5) //isn't this actually left (assuming camera looks out from you)?
      val up = (forward cross right).norm().times(1.5)
      Camera(pos, forward, up, right)
    }
  }

  case class Light(val src:Vector, val colour:Colour)

  case class Sphere(val centre:Vector, val rad:Double, val surface:Surface) extends SceneObject {

    def intersect(r:Ray):ISect = {
      val eo = centre - r.src
      val v = eo dot r.dir
      val dist:Double = {
        if (v < 0) 0
        else {
          val disc = Math.pow(rad, 2) - ((eo dot eo) - Math.pow(v, 2))
          if (disc < 0) 0 else v - Math.sqrt(disc)
        }
      }
      if (dist == 0) null
      else ISect(this, r, dist)
    }

    def normal(pos:Vector) = {
      (pos - centre).norm()
    }
  }

  case class Plane(val n:Vector, val offset:Double, val surface:Surface) extends SceneObject {

    def intersect(r:Ray) = {
      val denom = n dot r.dir
      if (denom > 0) null
      else ISect(this, r, ((n dot r.src) + offset) / (-denom))
    }

    def normal(pos:Vector) = n
  }

  class Scene(val objects:QVector[SceneObject], val lights:QVector[Light], val camera:Camera)

  class RayTracer(val screenWidth:Int, val screenHeight:Int, val maxDepth:Int) {

    val image = {
      val result = new BufferedImage(screenWidth, screenHeight, BufferedImage.TYPE_INT_RGB)
      result.createGraphics()
      result
    }

    val defaultObjects = {
      val objects = new QVector[SceneObject]
      objects += Plane(Vector(0, 1, 0), 0, Surface.checkerboard)
      objects += Sphere(Vector(0, 1, 0), 1, Surface.shiny)
      objects += Sphere(Vector(-2.25, .6, 1.5), .6, Surface.shiny)
      objects += Sphere(Vector(-3, 2.2, 0), .5, Surface.shiny)
      objects += Sphere(Vector(-1.75, 1.5, -1), 0.5, Surface.shiny)
      objects
    }

    val defaultLights = {
      val lights = new QVector[Light]
      lights += Light(Vector(-2, 2.5, 0), Colour(.49, .07, .07))
      lights += Light(Vector(1.5, 2.5, 1.5), Colour(.07, .07, .49))
      lights += Light(Vector(1.5, 2.5, -1.5), Colour(.07, .49, .071))
      lights += Light(Vector(0, 3.5, 0), Colour(.21, .21, .35))
      lights
    }

    val defaultScene = new Scene(defaultObjects, defaultLights, Camera.create(Vector(3, 2, 4), Vector(-1, .5, 0)))

    def intersections(r: Ray, s:Scene) = s.objects select(_.intersect(r)) where(_ != null) orderby(_.dist)

    def testRay(r: Ray, s:Scene):Double = {
      intersections(r, s) firstOrDefault match {
        case Some(hit) => hit.dist
        case None => 0
      }
    }

    def traceRay(r: Ray, s:Scene, depth:Int):Colour = {
      intersections(r, s) firstOrDefault match {
        case Some(hit) => shade(hit, s, depth)
        case None => Colour.background
      }
    }

    def getNaturalColour(obj:SceneObject, pos:Vector, norm:Vector, rd:Vector, scene:Scene) = {
      var ret = Colour.background
      val fn = (light:Light) => {
        val ldis = light.src - pos
        val livec = ldis.norm()
        val neatIsect = testRay(Ray(pos, livec), scene)
        val isInShadow = if (neatIsect > ldis.mag() || neatIsect == 0) false else true
        if (!isInShadow) {
          val illum = livec dot norm
          val lcolour = if (illum > 0) light.colour times illum else Colour.background
          val spec = livec dot (rd.norm())
          val scolor = if (spec > 0) light.colour times Math.pow(spec, obj.surface.roughness) else Colour.background
          ret = ret plus ((lcolour times obj.surface.diff(pos)) plus (scolor times obj.surface.spec(pos)))
        }
      }
      scene.lights.foreach(fn)
      ret
    }

    def getReflectionColour(obj:SceneObject, pos:Vector, norm:Vector, rd:Vector, scene:Scene, depth:Int) = {
      traceRay(Ray(pos, rd), scene, depth + 1).times(obj.surface.refl(pos))
    }

    def shade(hit:ISect, scene:Scene, depth:Int) = {
      val d = hit.ray.dir
      val pos = hit.ray.src + (hit.ray.dir times hit.dist)
      val normal = hit.obj.normal(pos)
      val reflDir = d - (normal times (2 * (normal dot d)))
      var ret = Colour.background
      ret = ret plus getNaturalColour(hit.obj, pos, normal, reflDir, scene)
      if (depth >= maxDepth) ret plus Colour(.5, .5, .5)
      else ret plus (getReflectionColour(hit.obj, pos + (reflDir times 0.001), normal, reflDir, scene, depth))
    }

    def recentreX(x:Double) = (x - (screenWidth / 2.0)) / (2.0 * screenWidth)
    def recentreY(y:Double) = -(y - (screenHeight / 2.0)) / (2.0 * screenHeight)
    def getPoint(x:Double, y:Double, camera:Camera) = {
      (camera.forward + (camera.right.times(recentreX(x)) + camera.up.times(recentreY(y)))).norm()
    }

    def render(scene: Scene) = {
      for (y <- 0 until screenHeight; x <- 0 until screenWidth) {
        val colour = traceRay(Ray(scene.camera.pos, getPoint(x, y, scene.camera)), scene, 0).legalize()
        image.setRGB(x, y, colour) //set pixel
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Scala Ray Tracer")
    frame.setPreferredSize(new Dimension(625, 650))

    class Renderer(val img:Image) extends Canvas {
      override def paint(g:Graphics) = {
        g.drawImage(img, 0, 0, this)
      }
    }

    val rt = new RayTracer(625, 650, 5)
    rt.render(rt.defaultScene)

    frame.add(new Renderer(rt.image))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack
    frame.setVisible(true)
  }
}

*/