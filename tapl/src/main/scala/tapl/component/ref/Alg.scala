package tapl.component.ref

import macros.Visitor
import tapl.common._

@Visitor
trait Alg[-R, E] {
  def tmRef(e: R): E

  def tmDeRef(e: R): E

  def tmAssign(l: R, r: R): E

  def tmLoc(x: String): E

  def apply(e: R): E
}

@Visitor
trait TAlg[-F, T] {
  def tyRef(t: F): T

  def apply(t: F): T
}
