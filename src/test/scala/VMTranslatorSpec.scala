import org.scalatest.FlatSpec

class VMTranslatorSpec extends  FlatSpec {

  "VMTranslator" should "execute parse memory commands and convert to asm" in {

    VMTranslator.main(Array("BasicMemoryCommand"))
  }

  "VMTranslator" should "execute parse push constnat 10 and convert to asm" in {

    VMTranslator.main(Array("pushconstant10"))
  }

  "VMTranslator" should "execute parse poplocal0 and convert to asm" in {

    VMTranslator.main(Array("poplocal0"))
  }

  "VMTranslator" should "execute parse popargument2 and convert to asm" in {

    VMTranslator.main(Array("popargument2"))
  }

  "VMTranslator" should "execute parse poptemp6 and convert to asm" in {

    VMTranslator.main(Array("poptemp6"))
  }

  "VMTranslator" should "execute parse popstatic8 and convert to asm" in {
    VMTranslator.main(Array("popstatic8"))
  }

  "VMTranslator" should "execute parse PointerTest and convert to asm" in {
    VMTranslator.main(Array("PointerTest"))
  }

  "VMTranslator" should "execute parse sub.vm and convert to asm" in {
    VMTranslator.main(Array("/machines/xenial64/Nand2Tetris/sub"))
  }

  "VMTranslator" should "execute parse BasicTest.vm and convert to asm" in {
    VMTranslator.main(Array("/machines/xenial64/Nand2Tetris/projects/07/MemoryAccess/BasicTest/BasicTest"))
  }

  "VMTranslator" should "execute parse StaticTest.vm and convert to asm" in {
    VMTranslator.main(Array("/machines/xenial64/Nand2Tetris/projects/07/MemoryAccess/StaticTest/StaticTest"))
  }

  "VMTranslator" should "execute parse PointerTest.vm and convert to asm" in {
    VMTranslator.main(Array("/machines/xenial64/Nand2Tetris/projects/07/MemoryAccess/PointerTest/PointerTest"))
  }

  "VMTranslator" should "execute parse SimpleAdd.vm and convert to asm" in {
    VMTranslator.main(Array("/machines/xenial64/Nand2Tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd"))
  }

  "VMTranslator" should "execute parse StackTest.vm and convert to asm" in {
    VMTranslator.main(Array("/machines/xenial64/Nand2Tetris/projects/07/StackArithmetic/StackTest/StackTest"))
  }
}
