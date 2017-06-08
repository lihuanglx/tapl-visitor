package tapl.component.record

import macros.Visitor
import tapl.common._

@Visitor
trait Alg[-R, E] {
  def tmRecord(l: List[(String, R)]): E

  def tmProj(e: R, x: String): E

  def apply(e: R): E
}
