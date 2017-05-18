package tapl.component.typed

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.varapp

trait Alg[-R, E, -F] extends varapp.Alg[R, E] {
  def TmAbs(x: String, t: F, e: R): E
}

trait TAlg[-F, T] {
  def TyArr(t1: F, t2: F): T

  def apply(t: F): T
}

trait Factory extends varapp.Factory {

  case class CAbs[A[-R, E, -F] <: Alg[R, E, F], V](x: String, t: V, e: E3[A, V]) extends E3[A, V] {
    override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.TmAbs(x, t, e)
  }

}

object Factory extends Factory

trait TFactory {

  case class CTyArr[A[-X, Y] <: TAlg[X, Y]](t1: Exp[A], t2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TyArr(t1, t2)
  }

}

object TFactory extends TFactory
