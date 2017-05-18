package tapl.component.typedbool

import tapl.common.Util._
import tapl.common.{Exp, TyperEq}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Type[B]] with TyperEq[B] {

  override def TmTrue(): Type[B] = CTyBool[B]()

  override def TmFalse(): Type[B] = CTyBool[B]()

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Type[B] = c => {
    val t1 = apply(e1)(c)
    t1 match {
      case CTyBool() =>
        val t2 = apply(e2)(c)
        val t3 = apply(e3)(c)
        if (t2(tEquals)(t3)) t2 else typeError()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyBool(): Exp[A] => Boolean = {
    case CTyBool() => true
    case _ => false
  }
}