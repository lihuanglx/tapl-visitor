package tapl.language.fullomega

import tapl.common._

import scala.collection.mutable

object Test {
  val parser = new Parse[Alg, TAlg, KAlg] {}

  type E = Exp3[Alg, Exp2[TAlg, Exp[KAlg]], Exp[KAlg]]

  def main(args: Array[String]): Unit = {
    val input = "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}} in (ops.f ops.c)"
    val ast: E = parser.parse(input).get
    println(ast(Print))
    println("Type: " + ast(Typer)(Ctx.empty())(Ctx.empty())(Ctx.empty())(TPrint))
    go(ast, mutable.MutableList(), 1)
  }

  def go(e: E, c: mutable.MutableList[E], step: Int): Unit = {
    println("Step " + step.toString + ": ")
    println("  Term: " + e(Print))
    if (e(IsVal)) {
      println("Value")
    } else {
      go(e(Eval)(c), c, step + 1)
    }
  }
}
