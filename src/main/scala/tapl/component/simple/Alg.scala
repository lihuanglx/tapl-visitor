package tapl.component.simple

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{floatstring, let, typed, typedrecord}
import tapl.language.tyarith

trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with tyarith.Alg[R, E]
  with floatstring.Alg[R, E] with let.Alg[R, E] with typedrecord.Alg[R, E] {

  def TmUnit(): E

  def TmAscribe(e: R, t: F): E

  def TmFix(e: R): E

  def TmInert(t: F): E
}

// todo: type vars
trait TAlg[-F, T] extends typed.TAlg[F, T] with tyarith.TAlg[F, T] with typedrecord.TAlg[F, T] {
  def TyUnit(): T

  def TyString(): T

  def TyFloat(): T
}

trait Factory extends typed.Factory with tyarith.Factory with floatstring.Factory
  with let.Factory with typedrecord.Factory {

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

}

object Factory extends Factory

trait TFactory extends typed.TFactory with typedrecord.TFactory with tyarith.TFactory {

  case class CTyUnit[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyUnit()
  }

  case class CTyString[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyString()
  }

  case class CTyFloat[A[-X, Y] <: TAlg[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyFloat()
  }

}

object TFactory extends TFactory
