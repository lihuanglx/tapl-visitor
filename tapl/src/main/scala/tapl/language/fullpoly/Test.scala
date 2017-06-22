package tapl.language.fullpoly

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg] {}

  def main(args: Array[String]): Unit = {
    //val input = "(\\f:Nat->Nat. f 2) ((\\X.\\x:X.x) [Nat])"
    val input = "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X, {c:X, f:X->Nat}} in (ops.f ops.c)"
    val ast: Exp2[Alg, Exp[TAlg]] = parser.parse(input).get
    println("Type: " + ast(Typer)(Ctx.empty())(TPrint))
    go(ast, 1)
  }

  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
}
