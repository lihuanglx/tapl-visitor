package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Alg[-R, E] extends bool.Alg[R, E] with nat.Alg[R, E]

trait Factory extends bool.Factory with nat.Factory

object Factory extends Factory

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}