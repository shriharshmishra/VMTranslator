import java.io._
import java.net.URL

import scala.io.Source

object FileManager {

  def readFile(path: URL): List[String] = {
    Source.fromURL(path).getLines()
      .map(_.trim) // remove whitespace before first character.
      .filter(line => !line.startsWith("//") && line.nonEmpty) // remove comments and empty lines
      .map(line => {
      if (line.contains("//")) line.substring(0, line.indexOf("//")).trim
      else line
    }) // remove comments after instructions
      .toList
  }

  def readFile(path: String): List[String] = {
    if (path.startsWith("/")) readFile(new URL(s"file://$path"))
    else readFile(getClass.getResource(path))
  }

  def writFile(path: String, lines: List[String]) {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))

    lines.foreach(line => {
      bw.write(line)
      bw.write("\n")
    })

    bw.close()
  }
}
