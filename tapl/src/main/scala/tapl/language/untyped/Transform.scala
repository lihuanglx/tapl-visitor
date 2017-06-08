package tapl.language.untyped

import tapl.common._
import tapl.component.varapp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with varapp.Transform[A] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs(x, apply(e))
}
