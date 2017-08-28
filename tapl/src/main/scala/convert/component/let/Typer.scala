package convert.component.let

import convert.common._

trait Typer[A[-X, Y] <: Term[X, Y], B[-X, Y]] extends Term[Exp[A], CtxTo[B]] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): CtxTo[B] = c => {
    val t = apply(e1)(c)
    apply(e2)(c + (x, t))
  }
}
