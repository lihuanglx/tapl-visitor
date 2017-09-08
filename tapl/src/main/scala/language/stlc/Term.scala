package language.stlc

import gems._

@Lang("stlc")
trait Term[-R, E, -T] {
  def tmVar(x: String): E

  def tmApp(e1: R, e2: R): E

  def tmAbs(x: String, t: T, e: R): E

  def tmUnit(): E

  def apply(e: R): E
}

@Lang("stlc")
trait Type[-R, E] {
  def tyArr(t1: R, t2: R): E

  def tyUnit(): E

  def apply(t: R): E
}
