package tapl.language.fullsimple

import tapl.common._
import tapl.component.{simple, variant}

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with simple.Print[A, V] with variant.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String] with simple.TPrint[A] with variant.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
