package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V]
  with tyarith.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with typed.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String]
  with tyarith.TPrint[A] with typed.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
