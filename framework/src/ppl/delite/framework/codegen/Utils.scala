package ppl.delite.framework.codegen

import java.io.{BufferedWriter, FileWriter, File}

/**
 * This trait is deprecated, and is only being used in old DSL implementations that have
 * not been updated to use DeliteStructs yet.
 */
trait Utils {
  def copyDataStructures(from: String, to: String, dsmap: String => String = s => s) {
    val dsDir = new File(from)
    if (!dsDir.exists) return
    val outDir = new File(to)
    outDir.mkdirs()
    copyDirectory(dsDir)

    def copyDirectory(dir: File) {
      for (f <- dir.listFiles) {
        if (f.isDirectory) 
          copyDirectory(f)
        else {
          val outFile = to + File.separator + f.getName
          val out = new BufferedWriter(new FileWriter(outFile))
          for (line <- io.Source.fromFile(f).getLines) {
            var remappedLine = dsmap(line)
            remappedLine = remappedLine.replaceAll("ppl.delite.framework.datastruct", "generated")
            out.write(remappedLine + System.getProperty("line.separator"))
          }
          out.close()
        }
      }
    }
  }
}
