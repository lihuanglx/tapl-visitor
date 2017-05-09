package tapl.component.typednat

import tapl.common.{Exp, TyperAux}
import tapl.component.typedbool
import tapl.component.typedbool.TFactory.CTyBool
import tapl.component.typednat.TFactory._

import scalaz.Scalaz._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y] with typedbool.TAlg[X, Y], M[_]]
  extends Alg[Exp[A], M[Exp[B]]] with TyperAux[B, M] {

  override def TmZero(): M[Exp[B]] = m.point(CTyNat[B]())

  override def TmPred(e: Exp[A]): M[Exp[B]] = for {
    t <- apply(e)
  } yield t match {
    case CTyNat() => t
    case _ => ??? //todo
  }

  override def TmSucc(e: Exp[A]): M[Exp[B]] = for {
    t <- apply(e)
  } yield t match {
    case CTyNat() => t
    case _ => ??? //todo
  }

  override def TmIsZero(e: Exp[A]): M[Exp[B]] = for {
    t <- apply(e)
  } yield t match {
    case CTyNat() => CTyBool[B]()
    case _ => ??? //todo
  }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyNat(): (Exp[A]) => Boolean = {
    case CTyNat() => true
    case _ => false
  }
}