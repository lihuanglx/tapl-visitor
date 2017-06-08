package tapl.language.bot

import tapl.common._
import tapl.component.{topbot, typed}

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with typed.Print[A, V]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with typed.TPrint[A] with topbot.TPrint[A]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

object TPrint extends TPrint[TAlg] {
  override def apply(t: Exp[TAlg]): String = t(this)
}