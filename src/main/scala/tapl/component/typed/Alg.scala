package tapl.component.typed

import tapl.common._
import tapl.component.varapp

trait Alg[-R, E, -F] extends varapp.Alg[R, E] {
  def TmAbs(x: String, t: F, e: R): E
}

trait TAlg[-F, T] {
  def TyArr(t1: F, t2: F): T

  def TyId(x: String): T

  def apply(t: F): T
}

case class CAbs[A[-R, E, -F] <: Alg[R, E, F], V](x: String, t: V, e: E3[A, V]) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmAbs(x, t, e)
}

trait Factory extends varapp.Factory {
  type CAbs[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.component.typed.CAbs[A, V]
  val CAbs = tapl.component.typed.CAbs
}

object Factory extends Factory

case class CTyArr[A[-X, Y] <: TAlg[X, Y]](t1: Exp[A], t2: Exp[A]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyArr(t1, t2)
}

case class CTyID[A[-X, Y] <: TAlg[X, Y]](x: String) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyId(x)
}

trait TFactory {
  type CTyArr[A[-X, Y] <: TAlg[X, Y]] = tapl.component.typed.CTyArr[A]
  val CTyArr = tapl.component.typed.CTyArr

  type CTyId[A[-X, Y] <: TAlg[X, Y]] = tapl.component.typed.CTyID[A]
  val CTyId = tapl.component.typed.CTyID
}

object TFactory extends TFactory
