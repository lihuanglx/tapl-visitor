package tapl.component.typedbool

import tapl.common._
import tapl.component.top
import tapl.component.top.CTyTop

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Type[B]] with ITEq[B] {

  override def tmTrue(): Type[B] = CTyBool[B]()

  override def tmFalse(): Type[B] = CTyBool[B]()

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Type[B] = c =>
    apply(e1)(c) match {
      case CTyBool() =>
        val t2 = apply(e2)(c)
        val t3 = apply(e3)(c)
        if (t2(tEquals)(t3)) t2 else typeError()
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyBool(): Exp[A] => Boolean = {
    case CTyBool() => true
    case _ => false
  }
}

trait Typer2[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Typer[A, B] with IJoin[B] {
  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Type[B] = c => {
    apply(e1)(c) match {
      case CTyBool() =>
        val t2 = apply(e2)(c)
        val t3 = apply(e3)(c)
        t2(join)(t3)
      case _ => typeError()
    }
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyBool(): (Exp[A]) => Boolean = {
    case CTyTop() => true
    case CTyBool() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def TyBool(): Exp[A] => Exp[A] = directJoin(CTyBool[A](), _).getOrElse(CTyTop[A]())
}
