package tapl.component.typed

import tapl.common.Util.E3
import tapl.common.{Exp, TyperAux}
import tapl.component.typed.TFactory._

import scalaz.Scalaz._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y], M[_]]
  extends Alg[E3[A, Exp[B]], M[Exp[B]], Exp[B]] with TyperAux[B, M] {

  override def TmAbs(x: String, t: Exp[B], e: E3[A, Exp[B]]): M[Exp[B]] = for {
  // todo
    tb <- apply(e)
  } yield CTyArr(t, tb)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case CTyArr(_t1, _t2) => apply(t1)(_t1) && apply(t2)(_t2)
    case _ => false
  }
}