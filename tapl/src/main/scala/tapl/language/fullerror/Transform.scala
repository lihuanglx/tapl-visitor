package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with bot.Alg.Transform[A, V] with typedbool.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmError(): TExp[A, V] = CError[A, V]()

  override def TmTry(e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] = CTry[A, V](apply(e1), apply(e2))
}
