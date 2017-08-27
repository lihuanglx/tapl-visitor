package inspect.component.nat

import inspect.common._
import macros.Lang

@Lang("nat")
trait Term[-R, E] {
  def tmZero(): E

  def tmSucc(e: R): E

  def tmPred(e: R): E

  def tmIsZero(e: R): E

  def apply(e: R): E
}
