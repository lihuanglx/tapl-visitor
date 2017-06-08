package tapl.component.let

import macros.Visitor
import tapl.common._

@Visitor
trait Alg[-R, E] {
  def tmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}
