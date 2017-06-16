package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed}
import tapl.language.equirec.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], Type[B], Exp[B]] with typed.Typer[A, B] with ISubst[B] {

  override def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): Type[B] = c => {
    def go(t: Exp[B]): Exp[B] = t match {
      case TyRec(x, r) => go(r(subst(x, t)))
      case TyArr(t1, t2) if tEquals(t1)(apply(e2)(c)) => t2
      case _ => typeError()
    }

    go(apply(e1)(c))
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean = _ (TEquals)(Set.empty)

  override def subst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]]
  extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean] with ISubst[A] {

  override def tyId(x: String): (Set[(Exp[A], Exp[A])]) => Exp[A] => Boolean = c => u => {
    val p = (TyId[A](x), u)
    c(p) || (u match {
      case TyId(y) => x == y
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

  override def tyVar(x: String): Set[(Exp[A], Exp[A])] => Exp[A] => Boolean = _ => _ => sys.error("impossible")
}

object TEquals extends TEquals[TAlg] with TImpl[Set[(Exp[TAlg], Exp[TAlg])] => Exp[TAlg] => Boolean] {
  override def subst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with rectype.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[TAlg]]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val m: Map[String, Exp[TAlg]] = mp
}
