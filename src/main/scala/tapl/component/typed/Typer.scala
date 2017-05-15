package tapl.component.typed

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.typed.TFactory._
import tapl.component.varapp

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with varapp.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def TmAbs(x: String, t: Exp[B], e: E3[A, Exp[B]]): Type[B] =
    c => CTyArr(t, apply(e)(c + (x, t)))
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case CTyArr(_t1, _t2) => apply(t1)(_t1) && apply(t2)(_t2)
    case _ => false
  }
}