package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V]
  with typedrecord.Print[A[-?, ?, V]] with bot.Print[A, V]

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String]
  with bot.TPrint[A] with typedrecord.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
