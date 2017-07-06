package tapl.component.typedbool

import tapl.common._
import tapl.component.top
import tapl.component.top.TAlg.Factory._
import tapl.component.typedbool.TAlg.Factory._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Exp[B]] with ITEq[B] {

  override def tmTrue(): Exp[B] = TyBool[B]()

  override def tmFalse(): Exp[B] = TyBool[B]()

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[B] =
    apply(e1) match {
      case TyBool() =>
        val t2 = apply(e2)
        val t3 = apply(e3)
        if (tEquals(t2)(t3)) t2 else typeError()
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyBool(): Exp[A] => Boolean = {
    case TyBool() => true
    case _ => false
  }
}

// subtyping
trait Typer2[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Typer[A, B] with IJoin[B] with ISubtypeOf[B] {
  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[B] =
    if (apply(e1)(subtypeOf)(TyBool())) {
      val t2 = apply(e2)
      val t3 = apply(e3)
      t2(join)(t3)
    } else typeError()
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyBool(): (Exp[A]) => Boolean = {
    case TyTop() => true
    case TyBool() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyBool(): Exp[A] => Exp[A] = directJoin(TyBool[A](), _).getOrElse(TyTop[A]())
}
