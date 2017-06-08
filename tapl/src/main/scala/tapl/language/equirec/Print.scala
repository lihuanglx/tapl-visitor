package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with typed.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String]
  with typed.TPrint[A] with rectype.TPrint[A] with typevar.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]