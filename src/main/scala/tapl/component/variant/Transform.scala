package tapl.component.variant

import tapl.common._

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V] {
  override def TmTag(x: String, e: E3[A, V], t: V): E3[A, V] = CTag[A, V](x, apply(e), t)

  override def TmCase(e: E3[A, V], l: List[(String, String, E3[A, V])]): E3[A, V] =
    CCase[A, V](apply(e), l map { case (a, b, c) => (a, b, apply(c)) })
}
