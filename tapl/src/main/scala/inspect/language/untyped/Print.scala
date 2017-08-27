package inspect.language.untyped

import inspect.common._
import inspect.component.varapp

trait Print[A[-R, _]] extends Term[Exp[A], String] with varapp.Print[A] {
  override def tmAbs(x: String, e: Exp[A]): String = "\\" + x + "." + apply(e)
}

object Print extends Print[Term] with Impl[String]
