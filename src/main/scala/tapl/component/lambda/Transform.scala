package tapl.component.lambda

import tapl.common.Exp
import tapl.component.lambda.Factory._

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs(x, apply(e))
}
