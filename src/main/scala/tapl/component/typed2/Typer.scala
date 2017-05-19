package tapl.component.typed2

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.typed

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed.Typer[A, B]

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with typed.TEquals[A] {
  // todo
  override def TyVar(x: String): (Exp[A]) => Boolean = {
    case CTyVar(y) => x == y
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with typed.SubtypeOf[A] {
  // todo
  override def TyVar(x: String): (Exp[A]) => Boolean = {
    case CTyVar(y) => x == y
    case _ => false
  }
}

trait Typer2[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends typed.Typer2[A, B]
