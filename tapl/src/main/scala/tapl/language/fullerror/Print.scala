package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V]
  with bot.Print[A, V] with typedbool.Print[A[-?, ?, V]] {

  override def tmError(): String = "error"

  override def tmTry(e1: Exp2[A, V], e2: Exp2[A, V]): String = "try " + apply(e1) + " with " + apply(e2)
}

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String] with bot.TPrint[A] with typedbool.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
