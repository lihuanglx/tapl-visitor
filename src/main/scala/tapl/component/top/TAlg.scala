package tapl.component.top

import tapl.common.Exp

trait TAlg[-F, T] {
  def TyTop(): T

  def apply(t: F): T
}

trait TFactory {

  case class CTyTop[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyTop()
  }

}

object TFactory extends TFactory
