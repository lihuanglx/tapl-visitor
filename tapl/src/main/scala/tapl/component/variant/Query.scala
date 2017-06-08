package tapl.component.variant

import tapl.common._

trait Query[R, E, F] extends Alg[R, E, F] with Default[E] {
  override def TmTag(x: String, e: R, t: F): E = default

  override def TmCase(e: R, l: List[(String, String, R)]): E = default
}
