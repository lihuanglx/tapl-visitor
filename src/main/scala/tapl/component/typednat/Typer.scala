package tapl.component.typednat

import tapl.common._
import tapl.component.top.CTyTop
import tapl.component.typedbool.TFactory._
import tapl.component.typednat.TFactory._
import tapl.component.{top, typedbool}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y] with typedbool.TAlg[X, Y]]
  extends Alg[Exp[A], Type[B]] {

  override def TmZero(): Type[B] = CTyNat[B]()

  override def TmPred(e: Exp[A]): Type[B] = c => {
    val t = apply(e)(c)
    t match {
      case CTyNat() => t
      case _ => typeError()
    }
  }

  override def TmSucc(e: Exp[A]): Type[B] = c => {
    val t = apply(e)(c)
    t match {
      case CTyNat() => t
      case _ => typeError()
    }
  }

  override def TmIsZero(e: Exp[A]): Type[B] = c => {
    val t = apply(e)(c)
    t match {
      case CTyNat() => CTyBool[B]()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyNat(): (Exp[A]) => Boolean = {
    case CTyNat() => true
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyNat(): (Exp[A]) => Boolean = {
    case CTyTop() => true
    case CTyNat() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def TyNat(): Exp[A] => Exp[A] = directJoin(CTyNat[A](), _).getOrElse(CTyTop[A]())
}
