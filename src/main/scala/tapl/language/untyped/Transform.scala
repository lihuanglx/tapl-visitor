package tapl.language.untyped

import tapl.common.Exp
import tapl.component.varapp
import tapl.language.untyped.Factory._

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with varapp.Transform[A] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs(x, apply(e))
}
