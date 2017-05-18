package tapl.component.simple

import tapl.component.{floatstring, let, typed, typedrecord}
import tapl.language.tyarith

trait Query[R, E, F] extends Alg[R, E, F] with floatstring.Query[R, E]
  with let.Query[R, E] with typed.Query[R, E, F] with typedrecord.Query[R, E] with tyarith.Query[R, E] {

  override def TmUnit(): E = default

  override def TmAscribe(e: R, t: F): E = default

  override def TmFix(e: R): E = default

  override def TmInert(t: F): E = default
}
