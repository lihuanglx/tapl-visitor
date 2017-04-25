package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]]
  with lambda.Transform[A] with varapp.Transform[A] {

  override val f: Factory[A]
}
