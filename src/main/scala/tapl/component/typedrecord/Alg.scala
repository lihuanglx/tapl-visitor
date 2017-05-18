package tapl.component.typedrecord

import tapl.common.Exp
import tapl.component.record

trait Alg[-R, E] extends record.Alg[R, E]

trait TAlg[-F, T] {
  def TyRecord(l: List[(String, F)]): T

  def apply(t: F): T
}

trait Factory extends record.Factory

object Factory extends Factory

case class CTyRecord[A[-X, Y] <: TAlg[X, Y]](l: List[(String, Exp[A])]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyRecord(l)
}

trait TFactory {
  type CTyRecord[A[-X, Y] <: TAlg[X, Y]] = tapl.component.typedrecord.CTyRecord[A]
  val CTyRecord = tapl.component.typedrecord.CTyRecord
}

object TFactory extends TFactory
