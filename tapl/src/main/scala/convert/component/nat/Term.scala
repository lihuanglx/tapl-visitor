package convert.component.nat

import convert.common._
import gems.Lang

@Lang("nat")
trait Term[-R, E] {
  def tmZero(): E

  def tmSucc(e: R): E

  def tmPred(e: R): E

  def tmIsZero(e: R): E

  def apply(e: R): E
}
