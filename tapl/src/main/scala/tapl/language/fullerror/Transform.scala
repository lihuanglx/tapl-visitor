package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with bot.Transform[A, V] with typedbool.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmError(): E3[A, V] = CError[A, V]()

  override def TmTry(e1: E3[A, V], e2: E3[A, V]): E3[A, V] = CTry[A, V](apply(e1), apply(e2))
}
