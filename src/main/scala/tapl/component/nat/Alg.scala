package tapl.component.nat

import tapl.common.Exp

trait Alg[-R, E] {
  def TmZero(): E

  def TmSucc(e: R): E

  def TmPred(e: R): E

  def TmIsZero(e: R): E

  def apply(e: R): E
}

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

trait Factory {
  type CZero[A[-X, Y] <: Alg[X, Y]] = tapl.component.nat.CZero[A]
  val CZero = tapl.component.nat.CZero

  type CSucc[A[-X, Y] <: Alg[X, Y]] = tapl.component.nat.CSucc[A]
  val CSucc = tapl.component.nat.CSucc

  type CPred[A[-X, Y] <: Alg[X, Y]] = tapl.component.nat.CPred[A]
  val CPred = tapl.component.nat.CPred

  type CIsZero[A[-X, Y] <: Alg[X, Y]]
  val CIsZero = tapl.component.nat.CIsZero
}

object Factory extends Factory
