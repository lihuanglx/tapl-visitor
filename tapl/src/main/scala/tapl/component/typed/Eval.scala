package tapl.component.typed

import tapl.common._
import tapl.component.typed.Alg.{Query, Transform}
import tapl.component.typed.Alg.Factory._
import tapl.component.varapp

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[Exp2[A, V], Exp2[A, V], V]
  with IIsVal[A[-?, ?, V]] with ISubst[A[-?, ?, V]] with varapp.Eval[A[-?, ?, V]] {

  override def tmAbs(x: String, t: V, e: Exp2[A, V]): Exp2[A, V] = TmAbs[A, V](x, t, e)

  override def tmApp(e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] =
    if (e1(isVal)) {
      if (e2(isVal)) e1 match {
        case TmAbs(x, _, body) => body(subst(x, e2))
        case _ => typeError()
      } else {
        TmApp[A[-?, ?, V]](e1, apply(e2))
      }
    } else {
      TmApp[A[-?, ?, V]](apply(e1), e2)
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V] with varapp.IsVal[A[-?, ?, V]] {
  override def tmAbs(x: String, t: V, e: Exp2[A, V]) = true
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with varapp.Subst[A[-?, ?, V]] {
  override def tmAbs(x: String, t: V, e: Exp2[A, V]): Exp2[A, V] =
    TmAbs[A, V](x, t, if (m.contains(x)) e else apply(e))
}
