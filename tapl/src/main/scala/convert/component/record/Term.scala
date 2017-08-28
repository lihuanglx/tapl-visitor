package convert.component.record

import macros.Lang
import convert.common._

@Lang("record")
trait Term[-R, E] {
  def tmRecord(l: List[(String, R)]): E

  def tmProj(e: R, x: String): E

  def apply(e: R): E
}
