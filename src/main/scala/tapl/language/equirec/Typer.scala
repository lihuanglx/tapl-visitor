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

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  val TEq: A[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]

  override def TyVar(x: String): Exp[A] => Boolean = _ => false

  override def TyId(x: String): (Exp[A]) => Boolean = TEq.TyId(x)(Set.empty)

  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = TEq.TyArr(t1, t2)(Set.empty)

  override def TyRec(x: String, t: Exp[A]): Exp[A] => Boolean = TEq.TyRec(x, t)(Set.empty)
}

trait TEq[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean] with ISubst[A] {
  override def TyId(x: String): (Set[(Exp[A], Exp[A])]) => Exp[A] => Boolean = c => u => {
    val p = (CTyId[A](x), u)
    c(p) || (u match {
      case CTyId(y) => x == y
      case CTyRec(_, _) => apply(u)(c)(p._1)
      case _ => false
    })
  }

  override def TyVar(x: String): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean = _ => _ => false

  override def TyRec(x: String, t: Exp[A]): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean = c => u => {
    val p = (CTyRec(x, t), u)
    c(p) || {
      val s = t(subst(x, p._1))
      apply(s)(c + p)(u)
    }
  }

  override def TyArr(t1: Exp[A], t2: Exp[A]): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean = c => u => {
    val p = (CTyArr(t1, t2), u)
    c(p) || (u match {
      case CTyArr(t3, t4) => apply(t1)(c)(t3) && apply(t2)(c)(t4)
      case CTyRec(_, _) => apply(u)(c)(p._1)
      case _ => apply(u)(c)(p._1)
    })
  }
}

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean] {
  override val TEq = tapl.language.equirec.TEq
}

object TEq extends TEq[TAlg] with TImpl[Set[(Exp[TAlg], Exp[TAlg])] => Exp[TAlg] => Boolean] {
  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with rectype.TSubst[A] with typevar.TSubst[A]

class TSubstImpl(_x: String, _e: Exp[TAlg]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val x: String = _x
  override val e: Exp[TAlg] = _e
}
