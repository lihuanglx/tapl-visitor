package tapl.language.untyped

import tapl.common._
import tapl.component.varapp

trait Print[A[-R, _]] extends Alg[Exp[A], String] with varapp.Print[A] {
  override def tmAbs(x: String, e: Exp[A]): String = "\\" + x + "." + apply(e)
}

object Print extends Print[Alg] with Impl[String]
