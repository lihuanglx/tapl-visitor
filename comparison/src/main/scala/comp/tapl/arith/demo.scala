package comp.tapl.arith

object ArithDemo extends comp.util.Demo[Unit, Command] {

  import Evaluator._
  import comp.util.Print._
  import PrettyPrinter._

  val width = 60

  override val initialContext: Unit = ()
  override val defaultExample: String = "examples/arith.tapl"

  override def parseInput(s: String): List[Command] =
    ArithParsers.input(s)

  override def processCommand(ctx: Unit, cmd: Command): Unit = cmd match {
    case Eval(t1) =>
      val doc1 = g2(ptmATerm(true, t1) :: ";")
      val t2 = eval(t1)
      val doc2 = g2(ptmATerm(true, t2) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))
  }

}
