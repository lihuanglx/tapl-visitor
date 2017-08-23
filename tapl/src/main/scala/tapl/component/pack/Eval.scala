package tapl.component.pack

import tapl.common._
import tapl.component.pack.Term.{Query, Transform}
import tapl.component.pack.Term.Factory._

trait Eval[A[-R, E, -F] <: Term[R, E, F], V] extends Term[Exp2[A, V], Exp2[A, V], V]
  with IIsVal[A[-?, ?, V]] with ISubst[A[-?, ?, V]] {

  override def tmPack(t1: V, e: Exp2[A, V], t2: V): Exp2[A, V] = TmPack(t1, apply(e), t2)

  override def tmUnpack(tx: String, x: String, e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] =
    if (e1(isVal)) e1 match {
      case TmPack(_, v, _) => e2(subst(x, v))
      case _ => typeError()
    }
    else TmUnpack(tx, x, apply(e1), e2)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V] {
  override val default: Boolean = false

  override def tmPack(t1: V, e: Exp2[A, V], t2: V): Boolean = apply(e)
}

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with SubstAux[A[-?, ?, V]] {
  override def tmUnpack(tx: String, x: String, e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] =
    if (m.contains(x)) TmUnpack(tx, x, apply(e1), e2) else TmUnpack(tx, x, apply(e1), apply(e2))
}
