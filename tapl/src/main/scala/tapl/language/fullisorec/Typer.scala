package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple
import tapl.language.fullisorec.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with fullsimple.Typer[A, B] with ISubst[B] {

  override def tmFold(e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = c => t match {
    case TyRec(x, u) if tEquals(apply(e)(c))(subst(x, t)(u)) => t
    case _ => typeError()
  }

  override def tmUnfold(e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = c => t match {
    case TyRec(x, u) if tEquals(apply(e)(c))(t) => subst(x, t)(u)
    case _ => typeError()
  }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean = _ (TEquals)(Set.empty)

  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Set[(String, String)] => Exp[A] => Boolean]
  with fullsimple.Type.Lifter[Exp[A], Exp[A] => Boolean, Set[(String, String)]] with ISubst[A] {

  override def propagate(c: Set[(String, String)]) = new fullsimple.TEquals[A] {
    override def apply(t: Exp[A]): (Exp[A]) => Boolean = TEquals.this.apply(t)(c)
  }

  override def tyRec(x: String, t: Exp[A]): Set[(String, String)] => Exp[A] => Boolean = c => {
    case TyRec(x2, t2) => apply(t)(c + (x -> x2))(t2)
    case _ => false
  }

  override def tyVar(x: String): Set[(String, String)] => Exp[A] => Boolean = c => {
    case TyVar(y) => (y == x) || c((x, y))
    case _ => false
  }
}

object TEquals extends TEquals[Type] with TImpl[Set[(String, String)] => Exp[Type] => Boolean] {
  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with rectype.TSubst[A] with fullsimple.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[Type]]) extends TSubst[Type] with TImpl[Exp[Type]] {
  override val m: Map[String, Exp[Type]] = mp
}
