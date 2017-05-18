package tapl.component.typed2

import tapl.common.Exp
import tapl.component.typed

trait Alg[-R, E, -F] extends typed.Alg[R, E, F]

trait TAlg[-F, T] extends typed.TAlg[F, T] {
  def TyVar(x: String): T
}

trait Factory extends typed.Factory

object Factory extends Factory

trait TFactory extends typed.TFactory {

  case class CTyVar[A[-X, Y] <: TAlg[X, Y]](x: String) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyVar(x)
  }

}

object TFactory extends TFactory
