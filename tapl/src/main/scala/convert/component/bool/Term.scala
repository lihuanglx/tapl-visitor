package convert.component.bool

import convert.common._
import gems.Lang

@Lang("bool")
trait Term[-R, E] {
  def tmTrue(): E

  def tmFalse(): E

  def tmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}
