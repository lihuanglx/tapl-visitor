package tapl.component.variant

import gems.Language
import tapl.common._

@Language
trait Term[-R, E, -F] {
  def tmTag(x: String, e: R, t: F): E

  def tmCase(e: R, l: List[(String, String, R)]): E

  def apply(e: R): E
}

@Language
trait Type[-F, T] {
  def tyVariant(l: List[(String, F)]): T

  def apply(t: F): T
}
