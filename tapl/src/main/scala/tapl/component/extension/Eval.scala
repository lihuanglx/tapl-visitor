package tapl.component.extension

import tapl.common._
import tapl.component.extension.Term.{Query, Transform}
import tapl.component._
import tapl.language.tyarith
import tapl.component.extension.Term.Factory._
import tapl.component.typed.Term.Factory.TmAbs

trait Eval[A[-R, E, -F] <: Term[R, E, F], V] extends Term[Exp2[A, V], Exp2[A, V], V]
  with tyarith.Eval[A[-?, ?, V]] with floatstring.Eval[A[-?, ?, V]]
  with let.Eval[A[-?, ?, V]] with typedrecord.Eval[A[-?, ?, V]] with unit.Eval[A[-?, ?, V]] {

  override def tmAscribe(e: Exp2[A, V], t: V): Exp2[A, V] = e

  override def tmFix(e: Exp2[A, V]): Exp2[A, V] =
    if (e(isVal)) e match {
      case TmAbs(x, t, b) => b(subst(x, TmFix[A, V](e)))
      case _ => typeError()
    } else {
      TmFix[A, V](apply(e))
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with tyarith.IsVal[A[-?, ?, V]] with floatstring.IsVal[A[-?, ?, V]]
  with typed.IsVal[A, V] with typedrecord.IsVal[A[-?, ?, V]] with unit.IsVal[A[-?, ?, V]]

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with let.Subst[A[-?, ?, V]]
