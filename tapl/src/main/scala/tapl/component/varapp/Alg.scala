package tapl.component.varapp

import tapl.common._
import macros.Visitor

@Visitor
trait Alg[-R, E] {
  def tmVar(x: String): E

  def tmApp(e1: R, e2: R): E

  def apply(e: R): E
}
