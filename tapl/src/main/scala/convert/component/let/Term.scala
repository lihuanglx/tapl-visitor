package convert.component.let

import gems.Lang
import convert.common._

@Lang("let")
trait Term[-R, E] {
  def tmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}
