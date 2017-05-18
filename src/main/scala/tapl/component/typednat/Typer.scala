package tapl.component.typednat

import tapl.common.Util._
import tapl.common.{Exp, TyperEq}
import tapl.component.typedbool
import tapl.component.typedbool.TFactory.CTyBool
import tapl.component.typednat.TFactory._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y] with typedbool.TAlg[X, Y]]
  extends Alg[Exp[A], Type[B]] with TyperEq[B] {

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