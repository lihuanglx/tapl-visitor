package tapl.component.unit

import tapl.common._
import macros.Visitor

@Visitor
trait Alg[-R, E] {
  def tmUnit(): E

  def apply(e: R): E
}

@Visitor
trait TAlg[-F, T] {
  def tyUnit(): T

  def apply(t: F): T
}
