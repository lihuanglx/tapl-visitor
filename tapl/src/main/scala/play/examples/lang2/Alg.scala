package play.examples.lang2

import play.examples.Common._
import macros.Visitor
import play.examples._

@Visitor
trait Alg[-R, E, -F] extends lang1.Alg[R, E] {
  def abs(x: String, t: F, e: R): E
}

/*
trait TAlg[-F, T] {
  def TyArr(t1: F, t2: F): T

  def TyId(x: String): T

  def apply(t: F): T
}
*/

case class CAbs[A[-R, E, -F] <: Alg[R, E, F], V](x: String, t: V, e: TExp[A, V]) extends TExp[A, V] {
  override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.abs(x, t, e)
}
