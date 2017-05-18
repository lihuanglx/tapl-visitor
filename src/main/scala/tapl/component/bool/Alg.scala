package tapl.component.bool

import tapl.common.Exp

trait Alg[-R, E] {
  def TmTrue(): E

  def TmFalse(): E

  def TmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}

case class CTrue[A[-X, Y] <: Alg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmTrue()
}

case class CFalse[A[-X, Y] <: Alg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmFalse()
}

case class CIf[A[-X, Y] <: Alg[X, Y]](e1: Exp[A], e2: Exp[A], e3: Exp[A]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmIf(e1, e2, e3)
}

trait Factory {
  type CTrue[A[-X, Y] <: Alg[X, Y]] = tapl.component.bool.CTrue[A]
  val CTrue = tapl.component.bool.CTrue

  type CFalse[A[-X, Y] <: Alg[X, Y]] = tapl.component.bool.CFalse[A]
  val CFalse = tapl.component.bool.CFalse

  type CIf[A[-X, Y] <: Alg[X, Y]] = tapl.component.bool.CIf[A]
  val CIf = tapl.component.bool.CIf
}

object Factory extends Factory
