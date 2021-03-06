package tapl.component.typed

import tapl.common._
import tapl.component.top.Type.Factory._
import tapl.component.typed.Type.Factory._
import tapl.component.{top, varapp}

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with varapp.Typer[A[-?, ?, Exp[B]], B] with ITEq[B] {

  override def tmAbs(x: String, t: Exp[B], e: Exp2[A, Exp[B]]): CtxTo[B] =
    c => TyArr(t, apply(e)(c + (x, t)))

  override def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c =>
    apply(e1)(c) match {
      case TyArr(t1, t2) if tEquals(t1)(apply(e2)(c)) => t2
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case TyArr(_t1, _t2) => apply(t1)(_t1) && apply(t2)(_t2)
    case _ => false
  }

  override def tyVar(x: String): Exp[A] => Boolean = {
    case TyVar(y) => x == y
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = {
    case TyTop() => true
    case TyArr(t3, t4) => apply(t3)(t1) && apply(t2)(t4)
    case _ => false
  }

  override def tyVar(x: String): Exp[A] => Boolean = {
    case TyTop() => true
    case TyVar(y) => x == y
    case _ => false
  }
}

trait Typer2[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Typer[A, B] with ISubtypeOf[B] {

  override def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c =>
    apply(e1)(c) match {
      case TyArr(t1, t2) => if (apply(e2)(c)(subtypeOf)(t1)) t2 else typeError()
      case _ => typeError()
    }
}

trait Join[A[-X, Y] <: Type[X, Y] with top.Type[X, Y]]
  extends Type[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {

  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Exp[A] = u =>
    directJoin(TyArr[A](t1, t2), u).getOrElse(u match {
      case TyArr(t3, t4) => TyArr[A](t1(meet)(t3), apply(t2)(t4))
      case _ => TyTop[A]()
    })

  override def tyVar(x: String): Exp[A] => Exp[A] =
    directJoin(TyVar[A](x), _).getOrElse(TyTop[A]())
}

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with SubstAux[A] {
  override def tyVar(x: String): Exp[A] = if (m.contains(x)) m(x) else TyVar[A](x)
}
