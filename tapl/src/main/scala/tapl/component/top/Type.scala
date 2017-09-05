package tapl.component.top

import gems.Language
import tapl.common._

@Language
trait Type[-F, T] {
  def tyTop(): T

  def apply(t: F): T
}
