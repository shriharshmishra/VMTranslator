import org.scalatest.FlatSpec

class CommandSpec extends FlatSpec {

  "Command" should " create new PushCommand which parses parts properly " in {

    val pop = new PopCommand("Foo", "pop local 2")

    pop.convert.foreach(println)
  }
}
