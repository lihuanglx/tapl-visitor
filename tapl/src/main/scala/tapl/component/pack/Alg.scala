package tapl.component.pack

import macros.Language
import tapl.common._

@Language
trait Alg[-R, E, -F] {
  def tmPack(t1: F, e: R, t2: F): E

  def tmUnpack(tx: String, x: String, e1: R, e2: R): E

  def apply(e: R): E
}
