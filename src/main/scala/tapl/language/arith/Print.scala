package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Print[A[-R, _]] extends Alg[Exp[A], String] with bool.Print[A] with nat.Print[A]

object PrintImpl extends Print[Alg] {
  override val isNumVal: Alg[Exp[Alg], Option[Int]] = new IsNumVal[Alg] {
    override def apply(e: Exp[Alg]): Option[Int] = e(this)
  }

  override def apply(e: Exp[Alg]): String = e(this)
}