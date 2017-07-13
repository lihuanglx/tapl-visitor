package tapl.language.fullref

import tapl.common._
import tapl.component.{variant, ref}
import tapl.language.fullsub

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V]
  with fullsub.Print[A, V] with variant.Print[A, V] with ref.Print[A[-?, ?, V]]

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String]
  with fullsub.TPrint[A] with variant.TPrint[A] with ref.TPrint[A] {

  override def tySource(t: Exp[A]): String = "Source " + apply(t)

  override def tySink(t: Exp[A]): String = "Sink " + apply(t)
}

object TPrint extends TPrint[TAlg] with TImpl[String]

