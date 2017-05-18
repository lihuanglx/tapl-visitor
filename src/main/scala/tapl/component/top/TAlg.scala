package tapl.component.top

import tapl.common.Exp

trait TAlg[-F, T] {
  def TyTop(): T

  def apply(t: F): T
}

case class CTyTop[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyTop()
}

trait TFactory {
  type CTyTop[A[-X, Y] <: TAlg[X, Y]] = tapl.component.top.CTyTop[A]
  val CTyTop = tapl.component.top.CTyTop
}

object TFactory extends TFactory
