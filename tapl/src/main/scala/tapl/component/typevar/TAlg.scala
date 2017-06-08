package tapl.component.typevar

import tapl.common.Exp

trait TAlg[-F, T] {
  def TyVar(x: String): T
}

case class CTyVar[A[-X, Y] <: TAlg[X, Y]](x: String) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyVar(x)
}

trait TFactory {
  type CTyVar[A[-X, Y] <: TAlg[X, Y]] = tapl.component.typevar.CTyVar[A]
  val CTyVar = tapl.component.typevar.CTyVar
}

object TFactory extends TFactory
