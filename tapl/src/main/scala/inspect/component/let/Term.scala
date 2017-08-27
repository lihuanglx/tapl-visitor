package inspect.component.let

import macros.Lang
import inspect.common._

@Lang("let")
trait Term[-R, E] {
  def tmLet(x: String, e1: R, e2: R): E

  def apply(e: R): E
}
