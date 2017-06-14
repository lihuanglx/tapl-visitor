package tapl.component.ref

import tapl.common._
import tapl.component.unit
import tapl.component.unit.TAlg.Factory._
import tapl.component.ref.TAlg.Factory._

trait Typer[A[-R, E] <: Alg[R, E], B[-X, Y] <: TAlg[X, Y] with unit.TAlg[X, Y]]
  extends Alg[Exp[A], (Ctx[Int, Exp[B]]) => Type[B]] with ITEq[B] {

  override def tmAssign(l: Exp[A], r: Exp[A]): (Ctx[Int, Exp[B]]) => Type[B] =
    c1 => c2 => apply(l)(c1)(c2) match {
      case TyRef(t) if tEquals(t)(apply(r)(c1)(c2)) => TyUnit[B]()
      case _ => typeError()
    }

  override def tmRef(e: Exp[A]): (Ctx[Int, Exp[B]]) => Type[B] =
    c1 => c2 => TyRef(apply(e)(c1)(c2))

  override def tmDeRef(e: Exp[A]): (Ctx[Int, Exp[B]]) => Type[B] =
    c1 => c2 => apply(e)(c1)(c2) match {
      case TyRef(t) => t
      case _ => typeError()
    }

  override def tmLoc(i: Int): (Ctx[Int, Exp[B]]) => Type[B] = c => _ => TyRef(c(i))
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyRef(t: Exp[A]): Exp[A] => Boolean = {
    case TyRef(t2) if apply(t)(t2) => true
    case _ => false
  }
}
