package gems.bool

import gems.common._

trait Impl[T] extends Term[Exp[Term], T] {
  def apply(e: Exp[Term]): T = e(this)
}

object Test extends App {

  val isVal = new IsVal[Term] with Impl[Boolean]

  val eval = new Eval[Term] with Impl[Exp[Term]] {
    def convertBool[B[-R, _]](e: SExp[Term, B]): Option[SExp[Term, B]] = Some(e)

    def isVal: Term[Exp[Term], Boolean] = Test.isVal
  }

  val print = new Print[Term] with Impl[String]

  val parser = new Parse[Term, Type] {}

  val input = "if false then false else (if false then false else true)"
  val ast: Exp[Term] = parser.parse(parser.pE)(input)

  def go(e: Exp[Term]): Exp[Term] = {
    println(e(print))
    if (e(isVal)) e else go(e(eval))
  }

  go(ast)
}
