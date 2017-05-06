package tapl.component.lambda

import tapl.common.Exp

trait Alg[-R, E] {
  def TmAbs(x: String, e: R): E

  def apply(e: R): E
}

trait Factory {

  case class CAbs[A[-X, Y] <: Alg[X, Y]](x: String, e: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmAbs(x, e)
  }

}

object Factory extends Factory
