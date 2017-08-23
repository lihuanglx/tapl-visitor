package tapl.component.varapp

import tapl.common._
import macros.Language

@Language
trait Term[-R, E] {
  def tmVar(x: String): E

  def tmApp(e1: R, e2: R): E

  def tmSeq(es: List[R]): E

  def apply(e: R): E
}
