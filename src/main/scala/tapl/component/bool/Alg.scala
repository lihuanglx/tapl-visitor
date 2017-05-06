package tapl.component.bool

import tapl.common.Exp

trait Alg[-R, E] {
  def TmTrue(): E

  def TmFalse(): E

  def TmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}

trait Factory {

  case class CTrue[A[-X, Y] <: Alg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmTrue()
  }

  case class CFalse[A[-X, Y] <: Alg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmFalse()
  }

  case class CIf[A[-X, Y] <: Alg[X, Y]](e1: Exp[A], e2: Exp[A], e3: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIf(e1, e2, e3)
  }

}

object Factory extends Factory
