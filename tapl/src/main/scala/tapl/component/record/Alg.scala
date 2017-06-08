package tapl.component.record

import tapl.common.Exp

trait Alg[-R, E] {
  def TmRecord(l: List[(String, R)]): E

  def TmProj(e: R, x: String): E

  def apply(e: R): E
}

case class CRecord[A[-X, Y] <: Alg[X, Y]](l: List[(String, Exp[A])]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmRecord(l)
}

case class CProj[A[-X, Y] <: Alg[X, Y]](e: Exp[A], x: String) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TmProj(e, x)
}

trait Factory {
  type CRecord[A[-X, Y] <: Alg[X, Y]] = tapl.component.record.CRecord[A]
  val CRecord = tapl.component.record.CRecord

  type CProj[A[-X, Y] <: Alg[X, Y]] = tapl.component.record.CProj[A]
  val CProj = tapl.component.record.CProj
}

object Factory extends Factory
