package tapl.component.lambda

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def TmAbs(x: String, e: Exp[A]): String = "\\" + x + "." + apply(e)
}
