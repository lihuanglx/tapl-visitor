package tapl.language.fullref

import tapl.common._
import tapl.component.{variant, ref}
import tapl.language.fullsub

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V]
  with fullsub.Print[A, V] with variant.Print[A, V] with ref.Print[A[-?, ?, V]]

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends Type[Exp[A], String]
  with fullsub.TPrint[A] with variant.TPrint[A] with ref.TPrint[A] {

  override def tySource(t: Exp[A]): String = "Source " + apply(t)

  override def tySink(t: Exp[A]): String = "Sink " + apply(t)
}

object TPrint extends TPrint[Type] with TImpl[String]

