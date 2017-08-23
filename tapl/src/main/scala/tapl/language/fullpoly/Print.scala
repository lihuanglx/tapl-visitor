package tapl.language.fullpoly

import tapl.common._
import tapl.component._

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V]
  with typed.Print[A, V] with extension.Print[A, V] with pack.Print[A, V] {

  override def tmTAbs(x: String, e: Exp2[A, V]): String = "\\" + x + "." + apply(e)

  override def tmTApp(e: Exp2[A, V], t: V): String = apply(e) + " [" + printT(t) + "]"
}

object Print extends Print[Term, Exp[Type]] with Impl[String] {
  override def printT(t: Exp[Type]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends Type[Exp[A], String]
  with typed.TPrint[A] with extension.TPrint[A] {

  override def tyAll(x: String, t: Exp[A]): String = "All " + x + "." + apply(t)

  override def tySome(x: String, t: Exp[A]): String = "{Some " + x + "," + apply(t) + "}"
}

object TPrint extends TPrint[Type] with TImpl[String]
