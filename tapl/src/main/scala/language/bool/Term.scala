package language.bool

import gems._

@Lang("bool")
trait Term[-R, E] {
  def tmTrue(): E

  def tmFalse(): E

  def tmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}

@Lang("bool")
trait Type[-R, E] {
  def tyBool(): E

  def apply(e: R): E
}
