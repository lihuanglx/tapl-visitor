package tapl.component.let

import tapl.common.Exp

trait Alg[-R, E] {
  def TmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}

trait Factory {

  case class CLet[A[-X, Y] <: Alg[X, Y]](x: String, e1: Exp[A], e2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmLet(x, e1, e2)
  }

}

object Factory extends Factory
