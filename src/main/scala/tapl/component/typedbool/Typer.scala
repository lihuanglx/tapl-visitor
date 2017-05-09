package tapl.component.typedbool

import tapl.common.{Exp, TyperAux}
import tapl.component.typedbool.TFactory._

import scalaz.Scalaz._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[B]]] with TyperAux[B, M] {
  override def TmTrue(): M[Exp[B]] = m.point(CTyBool[B]())

  override def TmFalse(): M[Exp[B]] = m.point(CTyBool[B]())

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): M[Exp[B]] = for {
    t1 <- apply(e1)
    t2 <- apply(e2)
    t3 <- apply(e3)
  } yield t1 match {
    case CTyBool() if t2(tEquals)(t3) => t2
    case _ => ??? //todo
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyBool(): Exp[A] => Boolean = {
    case CTyBool() => true
    case _ => false
  }
}