package tapl.component.variant

import tapl.common._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with EvalSubst[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmTag(x: String, e: E3[A, V], t: V): E3[A, V] =
    CTag[A, V](x, if (e(isVal)) e else apply(e), t)

  override def TmCase(e: E3[A, V], l: List[(String, String, E3[A, V])]): E3[A, V] =
    if (e(isVal)) e match {
      case CTag(n, v, _) =>
        val (_, x, b) = l.find(_._1 == n).getOrElse(typeError())
        b(subst(x, v))
      case _ => typeError()
    } else {
      CCase[A, V](apply(e), l)
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V] {
  override val default: Boolean = false

  override def TmTag(x: String, e: E3[A, V], t: V): Boolean = apply(e)
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with SubstAux[({type lam[-X, Y] = A[X, Y, V]})#lam] {
  override def TmCase(e: E3[A, V], l: List[(String, String, E3[A, V])]): E3[A, V] =
    CCase[A, V](apply(e), l.map({ case (a, b, c) => (a, b, if (b == this.x) c else apply(c)) }))
}
