package tapl.component.unit

import tapl.common._
import macros.Language

@Language
trait Term[-R, E] {
  def tmUnit(): E

  def apply(e: R): E
}

@Language
trait Type[-F, T] {
  def tyUnit(): T

  def apply(t: F): T
}
