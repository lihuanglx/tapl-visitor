package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with fullsimple.Print[A, V] {
  override def TmFold(e: E3[A, V], t: V): String = "fold [" + printT(t) + "] " + apply(e)

  override def TmUnfold(e: E3[A, V], t: V): String = "unfold [" + printT(t) + "] " + apply(e)
}

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with fullsimple.TPrint[A] with rectype.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
