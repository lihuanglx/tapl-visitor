package tapl.component.simple

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{floatstring, let, typed2, typedrecord}
import tapl.language.tyarith

trait Alg[-R, E, -F] extends typed2.Alg[R, E, F] with tyarith.Alg[R, E]
  with floatstring.Alg[R, E] with let.Alg[R, E] with typedrecord.Alg[R, E] {

  def TmUnit(): E

  def TmAscribe(e: R, t: F): E

  def TmFix(e: R): E

  def TmInert(t: F): E
}

trait TAlg[-F, T] extends typed2.TAlg[F, T] with tyarith.TAlg[F, T] with typedrecord.TAlg[F, T] {
  def TyUnit(): T

  def TyString(): T

  def TyFloat(): T
}

case class CUnit[A[-R, E, -F] <: Alg[R, E, F], V]() extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmUnit()
}

case class CAscribe[A[-R, E, -F] <: Alg[R, E, F], V](e: E3[A, V], t: V) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmAscribe(e, t)
}

case class CFix[A[-R, E, -F] <: Alg[R, E, F], V](e: E3[A, V]) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmFix(e)
}

case class CInert[A[-R, E, -F] <: Alg[R, E, F], V](t: V) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmInert(t)
}

trait Factory extends typed2.Factory with tyarith.Factory with floatstring.Factory
  with let.Factory with typedrecord.Factory {

  type CUnit[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.component.simple.CUnit[A, V]
  val CUnit = tapl.component.simple.CUnit

  type CAscribe[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.component.simple.CAscribe[A, V]
  val CAscribe = tapl.component.simple.CAscribe

  type CFix[A[-R, E, -F] <: Alg[R, E, F], V]
  val CFix = tapl.component.simple.CFix

  type CInert[A[-R, E, -F] <: Alg[R, E, F], V]
  val CInert = tapl.component.simple.CInert
}

object Factory extends Factory

case class CTyUnit[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyUnit()
}

case class CTyString[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyString()
}

case class CTyFloat[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyFloat()
}

trait TFactory extends typed2.TFactory with typedrecord.TFactory with tyarith.TFactory {
  type CTyUnit[A[-X, Y] <: TAlg[X, Y]] = tapl.component.simple.CTyUnit[A]
  val CTyUnit = tapl.component.simple.CTyUnit

  type CTyString[A[-X, Y] <: TAlg[X, Y]] = tapl.component.simple.CTyString[A]
  val CTyString = tapl.component.simple.CTyString

  type CTyFloat[A[-X, Y] <: TAlg[X, Y]] = tapl.component.simple.CTyFloat[A]
  val CTyFloat = tapl.component.simple.CTyFloat
}

object TFactory extends TFactory
