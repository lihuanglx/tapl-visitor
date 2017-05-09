package tapl.component.typednat

import tapl.common.Exp
import tapl.component.nat

trait Alg[-R, E] extends nat.Alg[R, E]

trait Factory extends nat.Factory

object Factory extends Factory

trait TAlg[-F, T] {
  def TyNat(): T

  def apply(t: F): T
}

trait TFactory {

  case class CTyNat[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyNat()
  }

}

object TFactory extends TFactory
