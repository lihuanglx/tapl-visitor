package tapl.language.fullerror

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.{typedbool, typevar}
import tapl.language.bot

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with bot.Print[A, V]
  with typedbool.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmError(): String = "error"

  override def TmTry(e1: E3[A, V], e2: E3[A, V]): String = "try " + apply(e1) + " with " + apply(e2)
}

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with bot.TPrint[A]
  with typevar.TPrint[A] with typedbool.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]