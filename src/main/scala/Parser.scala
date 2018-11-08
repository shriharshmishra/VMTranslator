object Parser {

  def parse(path: String, instructions: List[String]): List[Command] = {
    val fileName = if (path.contains("/")) path.split("/").last else path
    instructions map (Command(fileName, _))
  }
}

