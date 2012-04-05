import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = println("hello world")
}
