package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}
import tapl.language.equirec.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed.Typer[A, B] with ISubst[B] {

  override def TmApp(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c => {
    def go(t: Exp[B]): Exp[B] = t match {
      case CTyRec(x, r) => go(r(subst(x, t)))
      case CTyArr(t1, t2) if t1(tEquals)(apply(e2)(c)) => t2
      case _ => typeError()
    }

    go(apply(e1)(c))
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = TEquals

  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.TEquals2[A] with rectype.TEquals[A] {

  override def TyVar(x: String): Exp[A] => Boolean = _ => false
}

trait RecEq[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  with typed.RecEq[A] with rectype.RecEq[A] {

  override def TyVar(x: String): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean = _ => _ => false
}

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean] {
  override val recEq = RecEq
}

object RecEq extends RecEq[TAlg] with TImpl[Set[(Exp[TAlg], Exp[TAlg])] => Exp[TAlg] => Boolean] {
  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with rectype.TSubst[A] with typevar.TSubst[A]

class TSubstImpl(_x: String, _e: Exp[TAlg]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val x: String = _x
  override val e: Exp[TAlg] = _e
}
