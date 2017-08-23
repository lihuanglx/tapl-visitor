package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V] with fullsimple.Print[A, V] {
  override def tmFold(e: Exp2[A, V], t: V): String = "fold [" + printT(t) + "] " + apply(e)

  override def tmUnfold(e: Exp2[A, V], t: V): String = "unfold [" + printT(t) + "] " + apply(e)
}

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String] with fullsimple.TPrint[A] with rectype.TPrint[A]

object TPrint extends TPrint[Type] with TImpl[String]
