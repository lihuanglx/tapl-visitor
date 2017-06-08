package tapl.language.fullsub

import tapl.common._
import tapl.component.{simple, top}

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V] with simple.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String] with simple.TPrint[A] with top.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
