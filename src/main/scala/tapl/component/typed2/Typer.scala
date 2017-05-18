package tapl.component.typed2

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.typed
import tapl.component.typed2.TFactory.CTyVar

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed.Typer[A, B]

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with typed.TEquals[A] {
  // todo
  override def TyVar(x: String): (Exp[A]) => Boolean = {
    case CTyVar(y) => x == y
    case _ => false
  }
}
