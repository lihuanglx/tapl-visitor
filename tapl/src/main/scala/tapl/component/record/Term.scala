package tapl.component.record

import gems.Language
import tapl.common._

@Language
trait Term[-R, E] {
  def tmRecord(l: List[(String, R)]): E

  def tmProj(e: R, x: String): E

  def apply(e: R): E
}
