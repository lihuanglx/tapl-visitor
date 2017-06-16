package tapl.language.fullomega

import tapl.common._

object Test {
  val parser = new Parse[Alg, TAlg, KAlg] {}

  def main(args: Array[String]): Unit = {
    val input = "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}} in (ops.f ops.c)"
    val ast: Exp3[Alg, Exp2[TAlg, Exp[KAlg]], Exp[KAlg]] = parser.parse(input).get
    println(ast(Print))
    //println("Type: " + ast(Typer)(Ctx.empty())(TPrint))
    //go(ast, 1)
  }
/*
  def go(e: Exp2[Alg, Exp[TAlg]], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval), step + 1)
    }
  }
  */
}
