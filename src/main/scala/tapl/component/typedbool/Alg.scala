package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Alg[-R, E] extends bool.Alg[R, E]

trait Factory extends bool.Factory

object Factory extends Factory

trait TAlg[-F, T] {
  def TyBool(): T
}

trait TFactory {

  case class CTyBool[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyBool()
  }

}

object TFactory extends TFactory
