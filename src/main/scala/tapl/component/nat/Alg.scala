package tapl.component.nat

import tapl.common.Exp

trait Alg[-R, E] {
  def TmZero(): E

  def TmSucc(e: R): E

  def TmPred(e: R): E

  def TmIsZero(e: R): E

  def apply(e: R): E
}

trait Factory {

  case class CZero[A[-X, Y] <: Alg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmZero()
  }

  case class CSucc[A[-X, Y] <: Alg[X, Y]](e: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmSucc(e)
  }

  case class CPred[A[-X, Y] <: Alg[X, Y]](e: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmPred(e)
  }

  case class CIsZero[A[-X, Y] <: Alg[X, Y]](e: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIsZero(e)
  }

}

object Factory extends Factory
