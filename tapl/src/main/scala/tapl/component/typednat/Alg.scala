package tapl.component.typednat

import tapl.common.Exp
import tapl.component.nat

trait Alg[-R, E] extends nat.Alg[R, E]

trait Factory extends nat.Alg.Factory

object Factory extends Factory

trait TAlg[-F, T] {
  def TyNat(): T

  def apply(t: F): T
}

case class CTyNat[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyNat()
}

trait TFactory {
  type CTyNat[A[-X, Y] <: TAlg[X, Y]] = tapl.component.typednat.CTyNat[A]
  val CTyNat = tapl.component.typednat.CTyNat
}

object TFactory extends TFactory
