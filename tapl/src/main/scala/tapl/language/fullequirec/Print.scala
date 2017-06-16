package tapl.language.fullequirec

import tapl.common._
import tapl.component.{extension, variant}
import tapl.language.equirec

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V]
  with equirec.Print[A, V] with extension.Print[A, V] with variant.Print[A, V]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String]
  with equirec.TPrint[A] with extension.TPrint[A] with variant.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
