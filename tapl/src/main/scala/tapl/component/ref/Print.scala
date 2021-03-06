package tapl.component.ref

import tapl.common._

trait Print[A[-R, E]] extends Term[Exp[A], String] {
  override def tmRef(e: Exp[A]): String = "ref " + apply(e)

  override def tmDeRef(e: Exp[A]): String = "!" + apply(e)

  override def tmAssign(l: Exp[A], r: Exp[A]): String = apply(l) + " := " + apply(r)

  override def tmLoc(i: Int): String = "@" + i.toString
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String] {
  override def tyRef(t: Exp[A]): String = "Ref " + apply(t)
}
