package tapl.language.arith

import tapl.common.Exp
import tapl.component._


trait Alg[-R, E] extends bool.Alg[R, E] with nat.Alg[R, E]

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with bool.Factory[A] with nat.Factory[A]

object Factory extends Factory[Alg] with Impl[Exp[Alg]]

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}