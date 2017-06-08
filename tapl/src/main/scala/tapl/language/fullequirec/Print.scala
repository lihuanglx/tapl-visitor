package tapl.language.fullequirec

import tapl.common._
import tapl.component.{rectype, typevar}
import tapl.language.fullsimple

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V] with fullsimple.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with fullsimple.TPrint[A]
  with typevar.TPrint[A] with rectype.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
