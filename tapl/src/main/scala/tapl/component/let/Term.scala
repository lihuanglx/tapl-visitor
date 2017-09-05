package tapl.component.let

import gems.Language
import tapl.common._

@Language
trait Term[-R, E] {
  def tmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}
