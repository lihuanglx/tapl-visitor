package convert.component.varapp

import convert.common._
import macros.Lang

@Lang("varapp")
trait Term[-R, E] {
  def tmVar(x: String): E

  def tmApp(e1: R, e2: R): E

  def tmSeq(es: List[R]): E

  def apply(e: R): E
}
