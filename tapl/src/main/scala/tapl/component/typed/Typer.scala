package tapl.component.typed

import tapl.common._
import tapl.component.rectype.TAlg.Factory._
import tapl.component.topbot.TAlg.Factory._
import tapl.component.typed.TAlg.Factory._
import tapl.component.{top, topbot, varapp}

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]]
    with varapp.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] with ITEq[B] {

  override def tmAbs(x: String, t: Exp[B], e: TExp[A, Exp[B]]): Type[B] =
    c => TyArr(t, apply(e)(c + (x, t)))

  override def tmApp(e1: TExp[A, Exp[B]], e2: TExp[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case TyArr(t1, t2) if t1(tEquals)(apply(e2)(c)) => t2
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case TyArr(_t1, _t2) => apply(t1)(_t1) && apply(t2)(_t2)
    case _ => false
  }

  override def tyId(x: String): Exp[A] => Boolean = {
    case TyId(y) => x == y
    case _ => false
  }
}

// with recursive type
trait TEquals2[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with IRecEq[A] {
  override def tyId(x: String): (Exp[A]) => Boolean = recEq.tyId(x)(Set.empty)

  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = recEq.tyArr(t1, t2)(Set.empty)
}

trait RecEq[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean] with ISubst[A] {
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
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case TyTop() => true
    case TyArr(t3, t4) => apply(t3)(t1) && apply(t2)(t4)
    case _ => false
  }

  override def tyId(x: String): Exp[A] => Boolean = {
    case TyTop() => true
    case TyId(y) => x == y
    case _ => false
  }
}

trait Typer2[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Typer[A, B] with ISubtypeOf[B] {

  override def tmApp(e1: TExp[A, Exp[B]], e2: TExp[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case TyArr(t1, t2) => if (apply(e2)(c)(subtypeOf)(t1)) t2 else typeError()
      case _ => typeError()
    }
}

trait Typer3[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y] with topbot.TAlg[X, Y]]
  extends Typer2[A, B] {

  override def tmApp(e1: TExp[A, Exp[B]], e2: TExp[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case TyBot() => TyBot[B]()
      case _ => super.tmApp(e1, e2)(c)
    }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]]
  extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {

  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Exp[A] = u =>
    directJoin(TyArr[A](t1, t2), u).getOrElse(u match {
      case TyArr(t3, t4) => TyArr[A](t1(meet)(t3), apply(t2)(t4))
      case _ => TyTop[A]()
    })

  override def tyId(x: String): Exp[A] => Exp[A] =
    directJoin(TyId[A](x), _).getOrElse(TyTop[A]())
}
