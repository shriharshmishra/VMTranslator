import scala.collection.mutable
import scala.collection.parallel.immutable

/**
  * Base trait which has methods to read parts of the VM command.
  * Defines basic contract of the Command instance to convert the VM command to List of Hack instructions.
  */
trait Command {
  def fullCommand: String

  def convert: List[String]

  def parts: List[String] = fullCommand.split(" ").toList

  def name: String = parts(0)


  // Basic stack pointer operations.
  def stackPointer: String = "@SP"

  /**
    * Decrement SP
    *
    * @return
    */
  def decrementSP: List[String] = List("//decrement SP", stackPointer, "M=M-1")

  /**
    * Increment SP
    *
    * @return
    */
  def incrementSP: List[String] = List("//increment SP", stackPointer, "M=M+1")

  /**
    * Global set of all the labels generated in translating VM code file.
    */
  val labelSet: mutable.HashSet[String] = new scala.collection.mutable.HashSet[String]()

  /**
    * Generates label with unique name.
    * Recursively checks if newly generated label already exists or not.
    *
    * @param prefix - Textual prefix which makes label readable.
    * @return a unique label string.
    */
  def generateLabel(prefix: String): String = {
    def newLabel: String = {
      val label = prefix + "_" + ((Math.random() * 1000).toInt)
      if (labelSet.contains(label)) newLabel
      else labelSet += label

      label
    }

    newLabel
  }
}

/**
  * Companion object to Command trait. Its apply method create the Command instance for a VM command.
  */
object Command {

  def apply(fileName: String, command: String): Command = {
    val name = command.split(" ")(0)
    if (name == "pop") new PopCommand(fileName, command)
    else if (name == "push") new PushCommand(fileName, command)
    else if (List("add", "sub").contains(name)) new ArithmeticCommand(command)
    else if (List("and", "or").contains(name)) new BooleanCommand(command)
    else if (List("gt", "lt", "eq").contains(name)) new LogicalCommand(command)
    else if ("label" == name) new LabelCommand(command)
    else if ("if-goto" == name) new IfGotoCommand(command)
    else if ("goto" == name) new GotoCommand(command)
    else new OneOpCommand(command)
  }
}

/**
  * Command class which converts two-operand arithmethic command to assembly code.
  *
  * @param fullCommand
  */
class ArithmeticCommand(val fullCommand: String) extends Command {
  override def convert: List[String] = {
    println(fullCommand)
    "//" + fullCommand :: List("//Start VM Command") ::: readSecondOpFromStack ::: addressOfFirstOp :::
      ("//Execute Arithemtic Func" :: arithematicFunctions(name)) ::: incrementSP ::: List("//End VM Command")
  }

  def readSecondOpFromStack: List[String] = List("//Read Second Operand", stackPointer, "A=M", "A=A-1", "D=M")

  def addressOfFirstOp: List[String] = List("//Address Of First Op", "A=A-1")

  //TODO replace this method with generic implementation of incrementSP and test the code
  override def incrementSP: List[String] = List("//Increment SP", "D=A+1", "@SP", "M=D")

  def arithematicFunctions: Map[String, List[String]] = Map(
    "add" -> List("M=M+D"),
    "sub" -> List("M=M-D")
  )
}

/**
  * Command class which converts two-operand logical command to assembly code.
  *
  * @param fullCommand
  */
class LogicalCommand(val fullCommand: String) extends Command {
  override def convert: List[String] = {
    println(fullCommand)
    val setTrueLabel = generateLabel("SET_TRUE")
    val incSPLabel = generateLabel("INC_SP")
    List("//" + fullCommand, "//Start VM Command") ::: readSecondOpFromStack ::: decrementSP ::: addressOfFirstOp :::
      ("//Execute Logical Func" :: logicalFunctions(setTrueLabel)(name)) ::: pushFalseToStack :::
      jumpToIncSP(incSPLabel) ::: pushTrueToStack(setTrueLabel) :::
      incrementSPWithLabel(incSPLabel) ::: List("//End VM Command")
  }

  def readSecondOpFromStack: List[String] = "//Read Second Operand" :: decrementSP ::: List("A=M", "D=M")

  def addressOfFirstOp: List[String] = List("//Address Of First Op", "A=M")

  def pushFalseToStack: List[String] = List("//push false to stack", stackPointer, "A=M", "M=0")

  def jumpToIncSP(incSPLabel: String): List[String] = List("//Jump to increment SP", s"@$incSPLabel", "0;JMP")

  def pushTrueToStack(setTrueLabel: String): List[String] = List("//push true to stack", s"($setTrueLabel)", "@SP", "A=M", "M=-1")

  def incrementSPWithLabel(incSPLabel: String): List[String] = s"($incSPLabel)" :: incrementSP

  def logicalFunctions(setTrueLabel: String): Map[String, List[String]] = Map(
    "eq" -> List("D=M-D", s"@$setTrueLabel", "D;JEQ"),
    "gt" -> List("D=M-D", s"@$setTrueLabel", "D;JGT"),
    "lt" -> List("D=M-D", s"@$setTrueLabel", "D;JLT"))
}

/**
  * Command class which converts two-operand Boolean command to assembly code.
  *
  * @param fullCommand
  */
class BooleanCommand(val fullCommand: String) extends Command {
  override def convert: List[String] = {
    println(fullCommand)
    val incSPLabel = generateLabel("INC_SP")
    List("//" + fullCommand, "//Start VM Command") ::: readSecondOpFromStack ::: decrementSP ::: addressOfFirstOp :::
      ("//Execute Logical Func" :: booleanFunctions(name)) ::: incrementSP ::: List("//End VM Command")
  }

  def readSecondOpFromStack: List[String] = "//Read Second Operand" :: decrementSP ::: List("A=M", "D=M")

  def addressOfFirstOp: List[String] = List("//Address Of First Op", "A=M")

  def booleanFunctions: Map[String, List[String]] = Map(
    "and" -> List("M=M&D"),
    "or" -> List("M=M|D")
  )
}

/**
  * Command class which converts one-operand VM command to assembly code.
  *
  * @param fullCommand
  */
class OneOpCommand(val fullCommand: String) extends Command {
  override def convert: List[String] = {
    println(fullCommand)
    List("//" + fullCommand, "//Start VM Command") ::: addressOfOp ::: func ::: incrementSP ::: List("//End VM Command")
  }

  def addressOfOp: List[String] = "//Read Operand" :: decrementSP ::: List("A=M")

  def func: List[String] = if (name == "not") List("//Not operand", "M=!M") else List("//Negate operand", "M=-M")

}


abstract class MemoryCommand extends Command {

  def segment: String = if (parts.size > 1) parts(1) else null
  def index: String = if (parts.size > 2) parts(2) else null

  def fileName: String

  def decrementStackPointer: List[String] = List(stackPointer, "M=M-1")

  def incrementStackPointer: List[String] = List(stackPointer, "M=M+1")

  def calculateSegmentPointer: List[String] = {
    segmentPointerFunctions(segment)(index)
  }

  def segmentPointerFunctions: Map[String, (String) => List[String]]
}

/**
  * addr = segPointer + i;  *SP = *addr; SP++
  *
  * @param fullCommand
  */
class PushCommand(val fileName: String, val fullCommand: String) extends MemoryCommand {

  override def segmentPointerFunctions: Map[String, (String) => List[String]] = Map(
    "local" -> ((i: String) => List("@LCL", "A=M", "D=A", s"@$i", "A=D+A")),
    "static" -> ((i: String) => List(s"@$fileName.$i")),
    "this" -> ((i: String) => List("@THIS", "A=M", "D=A", s"@$i", "A=D+A")),
    "that" -> ((i: String) => List("@THAT", "A=M", "D=A", s"@$i", "A=D+A")),
    "argument" -> ((i: String) => List("@ARG", "A=M", "D=A", s"@$i", "A=D+A")),
    "constant" -> ((i: String) => List(s"@$i", "D=A")),
    "temp" -> ((i: String) => List("@5", "D=A", s"@$i", "A=D+A")),
    "pointer" -> ((i: String) => List(if (i == "0") "@THIS" else "@THAT")))

  override def convert: List[String] = {
    println(fullCommand)
    "// " + fullCommand :: (calculateSegmentPointer :::
      (if (segment == "constant") assignToStack drop 1 else assignToStack) ::: incrementStackPointer)
  }

  private def assignToStack: List[String] = List("D=M", "@SP", "A=M", "M=D")
}


/**
  *
  * addr = segPointer + i;   SP--; *addr= *SP;
  *
  * @param fullCommand
  */
class PopCommand(val fileName: String, val fullCommand: String) extends MemoryCommand {

  val saveAtR13: List[String] = List("@R13", "M=D")

  override def segmentPointerFunctions: Map[String, (String) => List[String]] = Map(
    "local" -> ((i: String) => List("@LCL", "A=M", "D=A", s"@$i", "D=D+A") ::: saveAtR13),
    "static" -> ((i: String) => List(s"@$fileName.$i", "D=A") ::: saveAtR13),
    "this" -> ((i: String) => List("@THIS", "A=M", "D=A", s"@$i", "D=D+A") ::: saveAtR13),
    "that" -> ((i: String) => List("@THAT", "A=M", "D=A", s"@$i", "D=D+A") ::: saveAtR13),
    "argument" -> ((i: String) => List("@ARG", "A=M", "D=A", s"@$i", "D=D+A") ::: saveAtR13),
    "constant" -> ((i: String) => List()),
    "temp" -> ((i: String) => List("@5", "D=A", s"@$i", "D=D+A") ::: saveAtR13),
    "pointer" -> ((i: String) => List(if (i == "0") "@THIS" else "@THAT", "D=A") ::: saveAtR13))

  override def convert: List[String] = {
    println(fullCommand)
    "// " + fullCommand :: (calculateSegmentPointer ::: decrementStackPointer ::: readFromStack ::: assignToMemory)
  }

  private def assignToMemory: List[String] = List("@R13", "A=M", "M=D")

  private def readFromStack: List[String] = List("A=M", "D=M")
}

class LabelCommand(val fullCommand: String) extends Command {
  override def convert: List[String] = List(s"// $fullCommand", s"(${parts(1)})")
}

class IfGotoCommand(val fullCommand: String) extends Command {

  def readFromStack = List(stackPointer, "A=M", "D=M")
  def loadLabelAddress = List(s"@${parts(1)}")
  def jumpIfGTZ = List("D;JGT")

  override def convert: List[String] = "//"+ fullCommand :: readFromStack ::: loadLabelAddress ::: jumpIfGTZ
}

class GotoCommand(val fullCommand: String) extends Command {

  def loadLabelAddress = List(s"@${parts(1)}")
  def jump = List("0;JMP")

  override def convert: List[String] = "//"+ fullCommand :: loadLabelAddress ::: jump
}
