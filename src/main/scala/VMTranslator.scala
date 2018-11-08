

object VMTranslator {

  def main(path: Array[String]): Unit = {
    val lines = FileManager.readFile(path(0))
    val linesAsm = Parser.parse(path(0), lines).flatMap(_.convert)

    val outputFile = path(0).substring(0, path(0).lastIndexOf(".") + 1) + "asm"
    FileManager.writFile(outputFile, linesAsm)
  }
}
