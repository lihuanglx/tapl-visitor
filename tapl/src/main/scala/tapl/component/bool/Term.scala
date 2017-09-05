package tapl.component.bool

import tapl.common._
import gems.Language

@Language
trait Term[-R, E] {
  def tmTrue(): E

  def tmFalse(): E

  def tmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}
