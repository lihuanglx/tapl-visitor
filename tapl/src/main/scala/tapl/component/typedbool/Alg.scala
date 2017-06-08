package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Alg[-R, E] extends bool.Alg[R, E]

trait Factory extends bool.Factory

object Factory extends Factory

trait TAlg[-F, T] {
  def TyBool(): T

  def apply(t: F): T
}

case class CTyBool[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyBool()
}

trait TFactory {
  type CTyBool[A[-X, Y] <: TAlg[X, Y]] = tapl.component.typedbool.CTyBool[A]
  val CTyBool = tapl.component.typedbool.CTyBool
}

object TFactory extends TFactory
