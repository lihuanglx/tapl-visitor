package tapl.component.pack

import tapl.common._
import tapl.component.pack.Alg.{Query, Transform}
import tapl.component.pack.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with IIsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] with ISubst[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmPack(t1: V, e: TExp[A, V], t2: V): TExp[A, V] = TmPack(t1, apply(e), t2)

  override def tmUnpack(tx: String, x: String, e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] =
    if (e1(isVal)) e1 match {
      case TmPack(_, v, _) => e2(subst(x, v))
      case _ => typeError()
    }
    else TmUnpack(tx, x, apply(e1), e2)
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V] {
  override val default: Boolean = false

  override def tmPack(t1: V, e: TExp[A, V], t2: V): Boolean = apply(e)
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V]
  extends Transform[A, V] with SubstAux[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmUnpack(tx: String, x: String, e1: TExp[A, V], e2: TExp[A, V]): TExp[A, V] =
    if (this.x == x) TmUnpack(tx, x, apply(e1), e2) else TmUnpack(tx, x, apply(e1), apply(e2))
}
