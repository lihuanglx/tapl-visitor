package tapl.component.let

import tapl.common._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y]] extends Alg[Exp[A], Type[B]] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): Type[B] = c => {
    val t = apply(e1)(c)
    apply(e2)(c + (x, t))
  }
}
