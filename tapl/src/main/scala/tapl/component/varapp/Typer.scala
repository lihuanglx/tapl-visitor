package tapl.component.varapp

import tapl.common._

trait Typer[A[-R, E] <: Alg[R, E], B[-X, Y]] extends Alg[Exp[A], Type[B]] {
  override def tmVar(x: String): Type[B] = c => c(x)

  override def tmSeq(es: List[Exp[A]]): Type[B] = c => es match {
    case a :: as => as.foldLeft(apply(a)(c))((r, e) => apply(e)(c))
    case _ => typeError()
  }
}
