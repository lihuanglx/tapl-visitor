package tapl.component.nat

import tapl.common._
import macros.Language

@Language
trait Term[-R, E] {
  def tmZero(): E

  def tmSucc(e: R): E

  def tmPred(e: R): E

  def tmIsZero(e: R): E

  def apply(e: R): E
}
