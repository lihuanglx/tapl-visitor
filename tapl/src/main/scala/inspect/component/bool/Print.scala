package inspect.component.bool

import inspect.common._

trait Print[A[-R, _]] extends Term[Exp[A], String] {
  override def tmTrue() = "true"

  override def tmFalse() = "false"

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): String =
    "if (" + apply(e1) + ") then (" + apply(e2) + ") else (" + apply(e3) + ")"
}
