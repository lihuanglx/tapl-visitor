package tapl.component.typed

import macros.Language
import tapl.common._
import tapl.component.varapp

@Language
trait Term[-R, E, -F] extends varapp.Term[R, E] {
  def tmAbs(x: String, t: F, e: R): E
}

@Language
trait Type[-F, T] {
  def tyArr(t1: F, t2: F): T

  def tyVar(x: String): T

  def apply(t: F): T
}
