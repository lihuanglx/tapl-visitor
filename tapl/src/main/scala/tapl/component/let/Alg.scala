package tapl.component.let

import tapl.common.Exp

trait Alg[-R, E] {
  def TmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}

case class CLet[A[-X, Y] <: Alg[X, Y]](x: String, e1: Exp[A], e2: Exp[A]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmLet(x, e1, e2)
}

trait Factory {
  type CLet[A[-X, Y] <: Alg[X, Y]] = tapl.component.let.CLet[A]
  val CLet = tapl.component.let.CLet
}

object Factory extends Factory
