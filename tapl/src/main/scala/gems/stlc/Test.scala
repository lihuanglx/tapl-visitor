package gems.stlc

import gems.common._

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait ImplT[T] extends Type[Exp[Type], T] {
  def apply(t: Exp[Type]): T = t(this)
}

object Test extends App {
  val isVal = new IsVal[Term, Exp[Type]] with Impl[Boolean]

  val eval = new Eval[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
    def subst(_u: String, _v: Exp2[Term, Exp[Type]]) =
      new Subst[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
        val u = _u
        val v = _v
      }

    def convertStlc[B[-R, _]](e: SExp2[Term, B, Exp[Type]]): Option[SExp2[Term, B, Exp[Type]]] =
      Some(e)

    def isVal: Term[Exp2[Term, Exp[Type]], Boolean, Exp[Type]] = Test.isVal
  }

  val print = new Print[Term, Exp[Type]] with Impl[String] {
    def printT(v: Exp[Type]): String = v(new PrintT[Type] with ImplT[String])
  }

  val tEquals = new TEquals[Type] with ImplT[Exp[Type] => Boolean] {
    def convertStlc[B[-R, _]](e: SExp[Type, B]): Option[SExp[Type, B]] = Some(e)
  }

  val typer = new Typer[Term, Type] with Impl[List[(String, Exp[Type])] => Exp[Type]] {
    def convertStlc[B[-R, _]](e: SExp[Type, B]): Option[SExp[Type, B]] = Some(e)

    def tEquals(t1: Exp[Type], t2: Exp[Type]): Boolean =
      t1(Test.tEquals)(t2)
  }

  val parser = new Parse[Term, Type] {}

  val input = "(\\f: Unit->Unit. f unit) (\\x: Unit. x)"
  val ast: Exp2[Term, Exp[Type]] = parser.parse(parser.pE)(input)

  def go(e: Exp2[Term, Exp[Type]]): Exp2[Term, Exp[Type]] = {
    println(e(print))
    if (e(isVal)) e else go(e(eval))
  }

  go(ast)
}

