package tapl.component.varapp

import tapl.common.Exp

trait Alg[-R, E] {
  def TmVar(x: String): E

  def TmApp(e1: R, e2: R): E

  def apply(e: R): E
}

case class CVar[A[-X, Y] <: Alg[X, Y]](x: String) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmVar(x)
}

case class CApp[A[-X, Y] <: Alg[X, Y]](e1: Exp[A], e2: Exp[A]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmApp(e1, e2)
}

trait Factory {
  type CVar[A[-X, Y] <: Alg[X, Y]] = tapl.component.varapp.CVar[A]
  val CVar = tapl.component.varapp.CVar

  type CApp[A[-X, Y] <: Alg[X, Y]] = tapl.component.varapp.CApp[A]
  val CApp = tapl.component.varapp.CApp
}

object Factory extends Factory
