package tapl.language.fullpoly

import tapl.common._
import tapl.component._

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V]
  with typed.Print[A, V] with extension.Print[A, V] with pack.Print[A, V] {

  override def tmTAbs(x: String, e: TExp[A, V]): String = "\\" + x + "." + apply(e)

  override def tmTApp(e: TExp[A, V], t: V): String = apply(e) + " [" + printT(t) + "]"
}

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-F, T]] extends TAlg[Exp[A], String]
  with typed.TPrint[A] with extension.TPrint[A] with typevar.TPrint[A] {

  override def tyAll(x: String, t: Exp[A]): String = "All " + x + "." + apply(t)

  override def tySome(x: String, t: Exp[A]): String = "{Some " + x + "," + apply(t) + "}"
}

object TPrint extends TPrint[TAlg] with TImpl[String]
