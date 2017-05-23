package tapl.component.typed

import tapl.common._
import tapl.component.top.TFactory.CTyTop
import tapl.component.topbot.CTyBot
import tapl.component.{top, topbot, varapp}

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]]
    with varapp.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] with ITEq[B] {

  override def TmAbs(x: String, t: Exp[B], e: E3[A, Exp[B]]): Type[B] =
    c => CTyArr(t, apply(e)(c + (x, t)))

  override def TmApp(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case CTyArr(t1, t2) if t1(tEquals)(apply(e2)(c)) => t2
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case CTyArr(_t1, _t2) => apply(t1)(_t1) && apply(t2)(_t2)
    case _ => false
  }

  override def TyId(x: String): Exp[A] => Boolean = {
    case CTyID(y) => x == y
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case CTyTop() => true
    case CTyArr(t3, t4) => apply(t3)(t1) && apply(t2)(t4)
    case _ => false
  }

  override def TyId(x: String): Exp[A] => Boolean = {
    case CTyTop() => true
    case CTyID(y) => x == y
    case _ => false
  }
}

trait Typer2[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Typer[A, B] with ISubtypeOf[B] {

  override def TmApp(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case CTyArr(t1, t2) => if (apply(e2)(c)(subtypeOf)(t1)) t2 else typeError()
      case _ => typeError()
    }
}

trait Typer3[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y] with topbot.TAlg[X, Y]]
  extends Typer2[A, B] {

  override def TmApp(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c =>
    apply(e1)(c) match {
      case CTyBot() => CTyBot[B]()
      case _ => super.TmApp(e1, e2)(c)
    }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]]
  extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {

  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Exp[A] = u =>
    directJoin(CTyArr[A](t1, t2), u).getOrElse(u match {
      case CTyArr(t3, t4) => CTyArr[A](t1(meet)(t3), apply(t2)(t4))
      case _ => CTyTop[A]()
    })

  override def TyId(x: String): Exp[A] => Exp[A] =
    directJoin(CTyID[A](x), _).getOrElse(CTyTop[A]())
}
