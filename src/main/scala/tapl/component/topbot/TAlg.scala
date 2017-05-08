package tapl.component.topbot

import tapl.common.Exp
import tapl.component.top

trait TAlg[-F, T] extends top.TAlg[F, T] {
  def TyBot(): T
}

trait TFactory {
  case class CTyBot[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyBot()
  }
}

object TFactory extends TFactory
