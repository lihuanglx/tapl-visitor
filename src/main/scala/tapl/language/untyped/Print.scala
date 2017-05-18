package tapl.language.untyped

import tapl.common.Exp
import tapl.component.varapp

trait Print[A[-R, _]] extends Alg[Exp[A], String] with varapp.Print[A] {
  override def TmAbs(x: String, e: Exp[A]): String = "\\" + x + "." + apply(e)
}

object Print extends Print[Alg] with Impl[String]
