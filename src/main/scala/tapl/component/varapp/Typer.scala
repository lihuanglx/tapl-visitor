package tapl.component.varapp

import tapl.common.Util._
import tapl.common.{Exp, TyperAux}
import tapl.component.typed.TFactory._

trait Typer[A[-R, E] <: Alg[R, E], B[-X, Y]] extends Alg[Exp[A], Type[B]] with TyperAux[B] {
  override def TmVar(x: String): Type[B] = c => c(x)

  override def TmApp(e1: Exp[A], e2: Exp[A]): Type[B] = c => {
    apply(e1)(c) match {
      case CTyArr(t1, t2) if t1(tEquals)(apply(e2)(c)) => t2
      case _ => typeError()
    }
  }
}
