package inspect.component.varapp

import inspect.common._

trait Typer[A[-R, E] <: Term[R, E], B[-X, Y]] extends Term[Exp[A], CtxTo[B]] {
  override def tmVar(x: String): CtxTo[B] = c => c(x)

  override def tmSeq(es: List[Exp[A]]): CtxTo[B] = c => es match {
    case a :: as => as.foldLeft(apply(a)(c))((r, e) => apply(e)(c))
    case _ => typeError()
  }
}
