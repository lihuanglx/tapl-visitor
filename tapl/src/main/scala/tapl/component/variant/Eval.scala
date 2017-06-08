package tapl.component.variant

import tapl.common._
import tapl.component.variant.Alg.{Query, Transform}
import tapl.component.variant.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with IIsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with ISubst[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmTag(x: String, e: TExp[A, V], t: V): TExp[A, V] =
    TmTag[A, V](x, if (e(isVal)) e else apply(e), t)

  override def tmCase(e: TExp[A, V], l: List[(String, String, TExp[A, V])]): TExp[A, V] =
    if (e(isVal)) e match {
      case TmTag(n, v, _) =>
        val (_, x, b) = l.find(_._1 == n).getOrElse(typeError())
        b(subst(x, v))
      case _ => typeError()
    } else {
      TmCase[A, V](apply(e), l)
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V] {
  override val default: Boolean = false

  override def tmTag(x: String, e: TExp[A, V], t: V): Boolean = apply(e)
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with SubstAux[({type lam[-X, Y] = A[X, Y, V]})#lam] {
  override def tmCase(e: TExp[A, V], l: List[(String, String, TExp[A, V])]): TExp[A, V] =
    TmCase[A, V](apply(e), l.map({ case (a, b, c) => (a, b, if (b == this.x) c else apply(c)) }))
}
