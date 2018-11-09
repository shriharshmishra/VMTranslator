import org.scalatest.FlatSpec

class CommandSpec extends FlatSpec {

//  "Command" should " create new PushCommand which parses parts properly " in {
//
//    val pop = new PopCommand("Foo", "pop local 2")
//
//    pop.convert.foreach(println)
//  }

  "Command" should " create a new LabelCommand when VM code has a label" in {
    assert(Command("filename", "label LOOP_START").isInstanceOf[LabelCommand])
  }

  "LabelCommand" should " convert when VM code to assembly" in {
    assert(Command("filename", "label LOOP_START").convert === List("// label LOOP_START", "(LOOP_START)"))
  }

  "Command" should " create a new IfGotCommand when VM code has a label" in {
    assert(Command("filename", "if-goto LOOP_START").isInstanceOf[IfGotoCommand])
  }

  "IfGotoCommand" should " convert when VM code to assembly" in {
    assert(Command("filename", "if-goto LOOP_START").convert ===
      List("//if-goto LOOP_START","@SP", "A=M", "D=M", "@LOOP_START", "D;JGT"))
  }
}
