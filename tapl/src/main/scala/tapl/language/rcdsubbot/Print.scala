package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V]
  with typedrecord.Print[A[-?, ?, V]] with bot.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String]
  with bot.TPrint[A] with typedrecord.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
