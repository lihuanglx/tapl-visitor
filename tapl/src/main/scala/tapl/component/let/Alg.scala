package tapl.component.let

import macros.Language
import tapl.common._

@Language
trait Alg[-R, E] {
  def tmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}
