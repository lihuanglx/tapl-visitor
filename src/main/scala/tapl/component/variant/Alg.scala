package tapl.component.variant

import tapl.common.Exp
import tapl.common.Util.E3

trait Alg[-R, E, -F] {
  def TmTag(x: String, e: R, t: F): E

  def TmCase(e: R, l: List[(String, String, R)]): E

  def apply(e: R): E
}

trait TAlg[-F, T] {
  def TyVariant(l: List[(String, F)]): T

  def apply(t: F): T
}

case class CTag[A[-R, E, -F] <: Alg[R, E, F], V](x: String, e: E3[A, V], t: V) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmTag(x, e, t)
}

case class CCase[A[-R, E, -F] <: Alg[R, E, F], V](e: E3[A, V], l: List[(String, String, E3[A, V])]) extends E3[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmCase(e, l)
}

trait Factory {
  type CTag[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.component.variant.CTag[A, V]
  val CTag = tapl.component.variant.CTag

  type CCase[A[-R, E, -F] <: Alg[R, E, F], V] = tapl.component.variant.CCase[A, V]
  val CCase = tapl.component.variant.CCase
}

object Factory extends Factory

case class CTyVariant[A[-X, Y] <: TAlg[X, Y]](l: List[(String, Exp[A])]) extends Exp[A] {
  override def apply[E](alg: A[Exp[A], E]): E = alg.TyVariant(l)
}

trait TFactory {
  type CTyVariant[A[-X, Y] <: TAlg[X, Y]] = tapl.component.variant.CTyVariant[A]
  val CTyVariant = tapl.component.variant.CTyVariant
}

object TFactory extends TFactory
