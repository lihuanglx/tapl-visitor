package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]]
  with typed.Typer[A, B] with extension.Typer[A, B] with ISubst[B] {

  override def tmTAbs(x: String, e: Exp2[A, Exp[B]]): CtxTo[B] = c => TyAll(x, apply(e)(c))

  override def tmTApp(e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = c => apply(e)(c) match {
    case TyAll(x, b) => b(subst(x, t))
    case _ => typeError()
  }

  override def tmPack(t1: Exp[B], e: Exp2[A, Exp[B]], t2: Exp[B]): CtxTo[B] = c => t2 match {
    case TySome(x, b) if tEquals(apply(e)(c))(b(subst(x, t1))) => t2
    case _ => typeError()
  }

  override def tmUnpack(tx: String, x: String, e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c =>
    apply(e1)(c) match {
      case TySome(y, b) => apply(e2)(c + (x -> b(subst(y, TyVar(tx)))))
      case _ => typeError()
    }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean = _ (TEquals)(Set.empty)

  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Set[(String, String)] => Exp[A] => Boolean]
  with typed.Type.Lifter[Exp[A], Exp[A] => Boolean, Set[(String, String)]]
  with extension.Type.Lifter[Exp[A], Exp[A] => Boolean, Set[(String, String)]] {

  override def propagate(c: Set[(String, String)]) = new typed.TEquals[A] with extension.TEquals[A] {
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
    case TyVar(y) => (y == x) || c((x, y))
    case _ => false
  }
}

object TEquals extends TEquals[Type] with TImpl[Set[(String, String)] => Exp[Type] => Boolean]

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with typed.TSubst[A] {
  override def tyAll(x: String, t: Exp[A]): Exp[A] = TyAll(x, if (m.contains(x)) t else apply(t))

  override def tySome(x: String, t: Exp[A]): Exp[A] = TySome(x, if (m.contains(x)) t else apply(t))
}

class TSubstImpl(mp: Map[String, Exp[Type]]) extends TSubst[Type] with TImpl[Exp[Type]] {
  override val m: Map[String, Exp[Type]] = mp
}
