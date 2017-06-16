package tapl.language.fullsimple

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    val input =
      """((\r: {x:Nat, y:Bool}.
        |  \v: <l:Nat, r:Nat>.
        |  case v of <l=lv> => if r.y then lv else r.x
        |          | <r=rv> => rv)
        |{x = 3, y = false})
        |(<l = 5> as <l:Nat, r:Nat>)
      """.stripMargin
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    go(ast, 1)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    println("  Type: " + e(Typer)(Ctx.empty())(TPrint))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
}
