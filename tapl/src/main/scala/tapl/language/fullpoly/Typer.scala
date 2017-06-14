package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]]
  with typed.Typer[A, B] with extension.Typer[A, B] with ISubst[B] {

  override def tmTAbs(x: String, e: TExp[A, Exp[B]]): Type[B] = c => TyAll(x, apply(e)(c))

  override def tmTApp(e: TExp[A, Exp[B]], t: Exp[B]): Type[B] = c => apply(e)(c) match {
    case TyAll(x, b) => b(subst(x, t))
    case _ => typeError()
  }

  override def tmPack(t1: Exp[B], e: TExp[A, Exp[B]], t2: Exp[B]): Type[B] = c => t2 match {
    case TySome(x, b) if tEquals(apply(e)(c))(b(subst(x, t1))) => t2
    case _ => typeError()
  }

  override def tmUnpack(tx: String, x: String, e1: TExp[A, Exp[B]], e2: TExp[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case TySome(y, b) => apply(e2)(c + (x -> b(subst(y, TyVar(tx)))))
      case _ => typeError()
    }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean = _ (TEquals)(Set.empty)

  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] = (x, e) => new TSubstImpl(x, e)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(String, String)] => Exp[A] => Boolean]
  with typed.TAlg.Lifter[Exp[A], Exp[A] => Boolean, Set[(String, String)]]
  with extension.TAlg.Lifter[Exp[A], Exp[A] => Boolean, Set[(String, String)]] {

  override def go(c: Set[(String, String)]) = new typed.TEquals[A] with extension.TEquals[A] {
    override def apply(t: Exp[A]): (Exp[A]) => Boolean = TEquals.this.apply(t)(c)
  }

  override def tyAll(x: String, t: Exp[A]): Set[(String, String)] => (Exp[A]) => Boolean = c => {
    case TyAll(x2, t2) => apply(t)(c + (x -> x2))(t2)
    case _ => false
  }

  override def tySome(x: String, t: Exp[A]): Set[(String, String)] => (Exp[A]) => Boolean = c => {
    case TySome(x2, t2) => apply(t)(c + (x -> x2))(t2)
    case _ => false
  }

  override def tyVar(x: String): Set[(String, String)] => (Exp[A]) => Boolean = c => {
    case TyVar(y) => c((x, y))
    case _ => false
  }
}

object TEquals extends TEquals[TAlg] with TImpl[Set[(String, String)] => Exp[TAlg] => Boolean]

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with typevar.TSubst[A] {
  override def tyAll(x: String, t: Exp[A]): Exp[A] = TyAll(x, if (this.x == x) t else apply(t))

  override def tySome(x: String, t: Exp[A]): Exp[A] = TySome(x, if (this.x == x) t else apply(t))
}

class TSubstImpl(_x: String, _e: Exp[TAlg]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val x: String = _x
  override val e: Exp[TAlg] = _e
}
