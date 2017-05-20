package tapl.component.varapp

import tapl.common._

trait Typer[A[-R, E] <: Alg[R, E], B[-X, Y]] extends Alg[Exp[A], Type[B]] with TyperAux[B] {
  override def TmVar(x: String): Type[B] = c => c(x)
}
