package tapl.component.varapp

import tapl.common.Util._
import tapl.common.{Exp, TyperAuxEq}

trait Typer[A[-R, E] <: Alg[R, E], B[-X, Y]] extends Alg[Exp[A], Type[B]] with TyperAuxEq[B] {
  override def TmVar(x: String): Type[B] = c => c(x)
}
