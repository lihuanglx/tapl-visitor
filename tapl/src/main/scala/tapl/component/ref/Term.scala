package tapl.component.ref

import gems.Language
import tapl.common._

@Language
trait Term[-R, E] {
  def tmRef(e: R): E

  def tmDeRef(e: R): E

  def tmAssign(l: R, r: R): E

  def tmLoc(i: Int): E

  def apply(e: R): E
}

@Language
trait Type[-F, T] {
  def tyRef(t: F): T

  def apply(t: F): T
}
