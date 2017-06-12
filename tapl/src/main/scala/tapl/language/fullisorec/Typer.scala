package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple
import tapl.language.fullisorec.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]] with fullsimple.Typer[A, B] with ISubst[B] {

  override def tmFold(e: TExp[A, Exp[B]], t: Exp[B]): Type[B] = c => t match {
    case TyRec(x, u) if tEquals(apply(e)(c))(subst(x, t)(u)) => t
    case _ => typeError()
  }

  override def tmUnfold(e: TExp[A, Exp[B]], t: Exp[B]): Type[B] = c => t match {
    case TyRec(x, u) if tEquals(apply(e)(c))(t) => subst(x, t)(u)
    case _ => typeError()
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean = _ (TEquals)(Set.empty)

  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(String, String)] => Exp[A] => Boolean]
  with fullsimple.TAlg.Lifter[Exp[A], Exp[A] => Boolean, Set[(String, String)]] with ISubst[A] {

  override def go(c: Set[(String, String)]) = new fullsimple.TEquals[A] {
    override def apply(t: Exp[A]): (Exp[A]) => Boolean = TEquals.this.apply(t)(c)
  }

  override def tyRec(x: String, t: Exp[A]): Set[(String, String)] => Exp[A] => Boolean = c => {
    case TyRec(x2, t2) => apply(t)(c + (x -> x2))(t2)
    case _ => false
  }

  override def tyVar(x: String): Set[(String, String)] => Exp[A] => Boolean = c => {
    case TyVar(y) => c((x, y))
    case _ => false
  }
}

object TEquals extends TEquals[TAlg] with TImpl[Set[(String, String)] => Exp[TAlg] => Boolean] {
  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with rectype.TSubst[A]

class TSubstImpl(_x: String, _e: Exp[TAlg]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val x: String = _x
  override val e: Exp[TAlg] = _e
}
