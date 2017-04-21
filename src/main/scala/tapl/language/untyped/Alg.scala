package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Alg [-R, E] extends lambda.Alg[R, E] with varapp.Alg[R, E]

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]]
  with lambda.Factory[A] with varapp.Factory[A]

object Factory extends Factory[Alg] with Impl[Exp[Alg]]

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}