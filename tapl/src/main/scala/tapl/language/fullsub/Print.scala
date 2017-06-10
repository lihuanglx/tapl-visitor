package tapl.language.fullsub

import tapl.common._
import tapl.component.{typed, extension, top}

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V]
  with typed.Print[A, V] with extension.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String]
  with typed.TPrint[A] with extension.TPrint[A] with top.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
