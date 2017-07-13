package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V] with let.Print[A[-?, ?, V]] with recon.Print[A, V] {
  override def tmUAbs(x: String, e: Exp2[A, V]): String = "\\" + x + "." + apply(e)
}

object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with recon.TPrint[A]

object TPrint extends TPrint[TAlg] with TImpl[String]
