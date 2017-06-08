package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Alg[-R, E, -F] extends fullsimple.Alg[R, E, F] {
  def TmFold(e: R, t: F): E

  def TmUnfold(e: R, t: F): E
}

case class CFold[A[-R, E, -F] <: Alg[R, E, F], V](e: TExp[A, V], t: V) extends TExp[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmFold(e, t)
}

case class CUnFold[A[-R, E, -F] <: Alg[R, E, F], V](e: TExp[A, V], t: V) extends TExp[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmUnfold(e, t)
}

trait Factory extends fullsimple.Alg.Factory {
  type CFold[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.language.fullisorec.CFold[A, V]
  val CFold = tapl.language.fullisorec.CFold

  type CUnFold[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.language.fullisorec.CUnFold[A, V]
  val CUnFold = tapl.language.fullisorec.CUnFold
}

object Factory extends Factory

trait TAlg[-F, T] extends fullsimple.TAlg[F, T] with rectype.TAlg[F, T]

trait TFactory extends fullsimple.TAlg.Factory with rectype.TAlg.Factory

object TFactory extends TFactory

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
