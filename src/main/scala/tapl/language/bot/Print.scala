package tapl.language.bot

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.{topbot, typed}

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with typed.Print[A, V]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with typed.TPrint[A] with topbot.TPrint[A]

object PrintImpl extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrintImpl)
}

object TPrintImpl extends TPrint[TAlg] {
  override def apply(t: Exp[TAlg]): String = t(this)
}
