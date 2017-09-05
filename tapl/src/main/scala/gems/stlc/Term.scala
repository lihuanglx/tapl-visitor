package gems.stlc

import gems.common._
import gems.Lang

@Lang("stlc")
trait Term[-R, E, -F] {
  def tmVar(x: String): E

  def tmApp(e1: R, e2: R): E

  def tmAbs(x: String, t: F, e: R): E

  def tmUnit(): E

  def apply(e: R): E
}

@Lang("stlc")
trait Type[-F, T] {
  def tyArr(t1: F, t2: F): T

  def tyUnit(): T

  def apply(t: F): T
}
