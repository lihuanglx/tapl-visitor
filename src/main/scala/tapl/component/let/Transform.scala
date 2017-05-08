package tapl.component.let

import tapl.common.Exp
import tapl.component.let.Factory._

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] = CLet(x, e1, e2)
}
