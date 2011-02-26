import scala.tools.nsc.Global
import tools.nsc.plugins.{PluginComponent, Plugin}
import tools.nsc.transform.Transform

/** A class describing the compiler plugin
 *
 *  @todo Adapt the name of this class to the plugin being
 *  implemented
 */
class QuerySyntaxPlugin(val global: Global) extends Plugin {
  val name = "query-syntax-transform"
  val description = "Translates query expressions to pure scala OptiQL embedded method calls"
  val components = List[PluginComponent](QuerySyntaxComponent)

  private object QuerySyntaxComponent extends PluginComponent with Transform {

    val runsAfter = List[String]("parser")
    override val runsBefore = List[String]("namer")
    val phaseName = QuerySyntaxPlugin.this.name

    override val optionsHelp = Some(
      "  -P:"+ name +":option     sets some option for this plugin\n"+
      "Valid Options:\n------------------\n"+
      "debug          outputs debug information, including tree browsers as transforms take place\n"+
      "enable         enable this plugin, by default, it is disabled")

    override def processOptions(options: List[String], error: String => Unit) {
      super.processOptions(options, error)
      //TODO need to process options
    }

    val global: QuerySyntaxPlugin.this.global.type = QuerySyntaxPlugin.this.global

    def newTransformer(unit: global.CompilationUnit) = QuerySyntaxTransformer

    object QuerySyntaxTransformer extends global.Transformer {
      override def transform(tree: global.Tree) = {
        QuerySyntaxComponent.this.global.treeBrowsers.create().browse(tree)
        tree
      }
    }
  }
}
