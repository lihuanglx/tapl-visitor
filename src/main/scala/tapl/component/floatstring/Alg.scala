package tapl.component.floatstring

import tapl.common.Exp

trait Alg[-R, E] {
  def TmFloat(d: Double): E

  def TmTimes(e1: R, e2: R): E

  def TmString(s: String): E

  def apply(e: R): E
}

trait Factory {

  case class CFloat[A[-X, Y] <: Alg[X, Y]](d: Double) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmFloat(d)
  }

  case class CTimes[A[-X, Y] <: Alg[X, Y]](e1: Exp[A], e2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmTimes(e1, e2)
  }

  case class CString[A[-X, Y] <: Alg[X, Y]](s: String) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmString(s)
  }

}

object Factory extends Factory
