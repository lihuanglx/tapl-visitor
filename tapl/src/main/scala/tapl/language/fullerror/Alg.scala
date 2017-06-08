package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

trait Alg[-R, E, -F] extends bot.Alg[R, E, F] with typedbool.Alg[R, E] {
  def TmError(): E

  def TmTry(e1: R, e2: R): E
}

trait TAlg[-F, T] extends bot.TAlg[F, T] with typedbool.TAlg[F, T]

case class CError[A[-R, E, -F] <: Alg[R, E, F], V]() extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmError()
}

case class CTry[A[-R, E, -F] <: Alg[R, E, F], V](e1: E3[A, V], e2: E3[A, V]) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmTry(e1, e2)
}

trait Factory extends bot.Factory with typedbool.Alg.Factory {
  type CError[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.language.fullerror.CError[A, V]
  val CError = tapl.language.fullerror.CError

  type CTry[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.language.fullerror.CTry[A, V]
  val CTry = tapl.language.fullerror.CTry
}

object Factory extends Factory

trait TFactory extends bot.TFactory with typedbool.TAlg.Factory

object TFactory extends TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
