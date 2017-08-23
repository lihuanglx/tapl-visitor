package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed}
import tapl.language.equirec.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with typed.Typer[A, B] with ISubst[B] {

  override def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c => {
    def go(t: Exp[B]): Exp[B] = t match {
      case TyRec(x, r) => go(r(subst(x, t)))
      case TyArr(t1, t2) if tEquals(t1)(apply(e2)(c)) => t2
      case _ => typeError()
    }

    go(apply(e1)(c))
  }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean = _ (TEquals)(Set.empty)

  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: Type[X, Y]]
  extends Type[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean] with ISubst[A] {

  override def tyVar(x: String): (Set[(Exp[A], Exp[A])]) => Exp[A] => Boolean = c => u => {
    val p = (TyVar[A](x), u)
    c(p) || (u match {
      case TyVar(y) => x == y
      case TyRec(_, _) => apply(u)(c)(p._1)
      case _ => false
    })
  }

  override def tyArr(t1: Exp[A], t2: Exp[A]): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean = c => u => {
    val p = (TyArr(t1, t2), u)
    c(p) || (u match {
      case TyArr(t3, t4) => apply(t1)(c)(t3) && apply(t2)(c)(t4)
      case TyRec(_, _) => apply(u)(c)(p._1)
      case _ => apply(u)(c)(p._1)
    })
  }

  override def tyRec(x: String, t: Exp[A]): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean =
    c => u => {
      val p = (TyRec(x, t), u)
      c(p) || {
        val s = t(subst(x, p._1))
        apply(s)(c + p)(u)
      }
    }
}

object TEquals extends TEquals[Type] with TImpl[Set[(Exp[Type], Exp[Type])] => Exp[Type] => Boolean] {
  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with rectype.TSubst[A] with typed.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[Type]]) extends TSubst[Type] with TImpl[Exp[Type]] {
  override val m: Map[String, Exp[Type]] = mp
}
