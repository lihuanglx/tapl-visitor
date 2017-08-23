package tapl.language.fullisorec

import tapl.common._
import tapl.language.fullsimple
import tapl.language.fullisorec.Term.{Query, Transform}
import tapl.language.fullisorec.Term.Factory._

trait Eval[A[-R, E, -F] <: Term[R, E, F], V] extends Term[Exp2[A, V], Exp2[A, V], V] with fullsimple.Eval[A, V] {
  override def tmFold(e: Exp2[A, V], t: V): Exp2[A, V] =
    TmFold[A, V](if (e(isVal)) e else apply(e), t)

  override def tmUnfold(e: Exp2[A, V], t: V): Exp2[A, V] =
    if (e(isVal)) e match {
      case TmFold(v, _) => v
      case _ => typeError()
    } else {
      TmUnfold[A, V](apply(e), t)
    }
}

object Eval extends Eval[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val isVal: Term[Exp2[Term, Exp[Type]], Boolean, Exp[Type]] = IsVal

  override def subst(m: Map[String, Exp2[Term, Exp[Type]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V] with fullsimple.IsVal[A, V] {
  override def tmFold(e: Exp2[A, V], t: V): Boolean = apply(e)
}

object IsVal extends IsVal[Term, Exp[Type]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with fullsimple.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Term, Exp[Type]]]) extends Subst[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val m: Map[String, Exp2[Term, Exp[Type]]] = mp
}

