package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Alg [-R, E] extends lambda.Alg[R, E] with varapp.Alg[R, E]

trait Factory extends lambda.Factory with varapp.Factory

object Factory extends Factory

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}