package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with typed.Print[A, V] with typedbool.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with typed.TPrint[A] with typedbool.TPrint[A]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

object TPrint extends TPrint[TAlg] {
  override def apply(t: Exp[TAlg]): String = t(this)
}
