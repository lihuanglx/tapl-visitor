package tapl.component.variant

import macros.Visitor
import tapl.common._

@Visitor
trait Alg[-R, E, -F] {
  def tmTag(x: String, e: R, t: F): E

  def tmCase(e: R, l: List[(String, String, R)]): E

  def apply(e: R): E
}

@Visitor
trait TAlg[-F, T] {
  def tyVariant(l: List[(String, F)]): T

  def apply(t: F): T
}
