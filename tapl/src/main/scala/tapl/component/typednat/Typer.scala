package tapl.component.typednat

import tapl.common._
import tapl.component.top.TAlg.Factory._
import tapl.component.typedbool.TAlg.Factory._
import tapl.component.typednat.TAlg.Factory._
import tapl.component.{top, typedbool}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y] with typedbool.TAlg[X, Y]]
  extends Alg[Exp[A], Type[B]] {

  override def tmZero(): Type[B] = TyNat[B]()

  override def tmPred(e: Exp[A]): Type[B] = c => {
    val t = apply(e)(c)
    t match {
      case TyNat() => t
      case _ => typeError()
    }
  }

  override def tmSucc(e: Exp[A]): Type[B] = c => {
    val t = apply(e)(c)
    t match {
      case TyNat() => t
      case _ => typeError()
    }
  }

  override def tmIsZero(e: Exp[A]): Type[B] = c => {
    val t = apply(e)(c)
    t match {
      case TyNat() => TyBool[B]()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyNat(): (Exp[A]) => Boolean = {
    case TyNat() => true
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyNat(): (Exp[A]) => Boolean = {
    case TyTop() => true
    case TyNat() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyNat(): Exp[A] => Exp[A] = directJoin(TyNat[A](), _).getOrElse(TyTop[A]())
}
