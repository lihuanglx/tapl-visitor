package tapl.language.fulluntyped

import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

trait Alg[-R, E] extends arith.Alg[R, E] with untyped.Alg[R, E]
  with record.Alg[R, E] with floatstring.Alg[R, E] with let.Alg[R, E]

trait Factory extends arith.Alg.Factory with untyped.Factory
  with record.Factory with floatstring.Factory with let.Factory

object Factory extends Factory

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}
