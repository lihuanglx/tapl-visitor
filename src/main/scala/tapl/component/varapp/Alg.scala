package tapl.component.varapp

import tapl.common.Exp

trait Alg[-R, E] {
  def TmVar(x: String): E

  def TmApp(e1: R, e2: R): E

  def apply(e: R): E
}

trait Factory {

  case class CVar[A[-X, Y] <: Alg[X, Y]](x: String) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmVar(x)
  }

  case class CApp[A[-X, Y] <: Alg[X, Y]](e1: Exp[A], e2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmApp(e1, e2)
  }

}

object Factory extends Factory
