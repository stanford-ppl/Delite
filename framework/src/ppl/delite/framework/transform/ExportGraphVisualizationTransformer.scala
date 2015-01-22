package ppl.delite.framework.transform

import ppl.delite.framework.ops.{DeliteOpsExp}
import reflect.{Manifest, SourceContext}
import java.io.PrintWriter
import scala.collection.mutable.{ArrayBuffer, Map}
import ppl.delite.framework.DeliteIRGraphExport

trait ExportGraphVisualizationTransformExp extends DeliteOpsExp { self =>
    class ExportGraphVisualizationTransformer(val stream: PrintWriter) extends ForwardPassTransformer {
        val IR: self.type = self

        override def toString = "ExportGraphVisualizationTransformer"

        override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
            val p = new DeliteIRGraphExport { val IR: self.type = self }
            p.exportGraph(b, stream)

            super.runOnce(b)
        }
    }
}
