package tapl.component.rectype

import tapl.common._

trait TAlg[-F, T] {
  def TyRec(x: String, t: F): T

  def apply(t: F): T
}

case class CTyRec[A[-X, Y] <: TAlg[X, Y]](x: String, t: Exp[A]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyRec(x, t)
}

trait TFactory {
  type CTyRec[A[-X, Y] <: TAlg[X, Y]] = tapl.component.rectype.CTyRec[A]
  val CTyRec = tapl.component.rectype.CTyRec
}
