package tapl.component.typednat

import tapl.common._
import tapl.component.top.Type.Factory._
import tapl.component.typedbool.Type.Factory._
import tapl.component.typednat.Type.Factory._
import tapl.component.{top, typedbool}

trait Typer[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y] with typedbool.Type[X, Y]]
  extends Term[Exp[A], Exp[B]] {

  override def tmZero(): Exp[B] = TyNat[B]()

  override def tmPred(e: Exp[A]): Exp[B] = {
    val t = apply(e)
    t match {
      case TyNat() => t
      case _ => typeError()
    }
  }

  override def tmSucc(e: Exp[A]): Exp[B] = {
    val t = apply(e)
    t match {
      case TyNat() => t
      case _ => typeError()
    }
  }

  override def tmIsZero(e: Exp[A]): Exp[B] = {
    val t = apply(e)
    t match {
      case TyNat() => TyBool[B]()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyNat(): (Exp[A]) => Boolean = {
    case TyNat() => true
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyNat(): (Exp[A]) => Boolean = {
    case TyTop() => true
    case TyNat() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: Type[X, Y] with top.Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyNat(): Exp[A] => Exp[A] = directJoin(TyNat[A](), _).getOrElse(TyTop[A]())
}
