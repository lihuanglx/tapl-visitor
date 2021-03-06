package tapl.component.variant

import tapl.common._
import tapl.component.variant.Term.{Query, Transform}
import tapl.component.variant.Term.Factory._

trait Eval[A[-R, E, -F] <: Term[R, E, F], V] extends Term[Exp2[A, V], Exp2[A, V], V]
  with IIsVal[A[-?, ?, V]] with ISubst[A[-?, ?, V]] {

  override def tmTag(x: String, e: Exp2[A, V], t: V): Exp2[A, V] =
    TmTag[A, V](x, if (e(isVal)) e else apply(e), t)

  override def tmCase(e: Exp2[A, V], l: List[(String, String, Exp2[A, V])]): Exp2[A, V] =
    if (e(isVal)) e match {
      case TmTag(n, v, _) =>
        val (_, x, b) = l.find(_._1 == n).getOrElse(typeError())
        b(subst(x, v))
      case _ => typeError()
    } else {
      TmCase[A, V](apply(e), l)
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V] {
  override val default: Boolean = false

  override def tmTag(x: String, e: Exp2[A, V], t: V): Boolean = apply(e)
}

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with SubstAux[A[-?, ?, V]] {
  override def tmCase(e: Exp2[A, V], l: List[(String, String, Exp2[A, V])]): Exp2[A, V] =
    TmCase[A, V](apply(e), l.map({ case (a, b, c) => (a, b, if (m.contains(b)) c else apply(c)) }))
}
