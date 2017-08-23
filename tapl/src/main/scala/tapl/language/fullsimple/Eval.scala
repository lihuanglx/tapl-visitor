package tapl.language.fullsimple

import tapl.common._
import tapl.component.{typed, extension, variant}
import tapl.language.fullsimple.Term._

trait Eval[A[-R, E, -F] <: Term[R, E, F], V] extends Term[Exp2[A, V], Exp2[A, V], V]
  with typed.Eval[A, V] with extension.Eval[A, V] with variant.Eval[A, V]

object Eval extends Eval[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val isVal: Term[Exp2[Term, Exp[Type]], Boolean, Exp[Type]] = IsVal

  override def subst(m: Map[String, Exp2[Term, Exp[Type]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with typed.IsVal[A, V] with extension.IsVal[A, V] with variant.IsVal[A, V]

object IsVal extends IsVal[Term, Exp[Type]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with
  typed.Subst[A, V] with extension.Subst[A, V] with variant.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Term, Exp[Type]]]) extends Subst[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val m: Map[String, Exp2[Term, Exp[Type]]] = mp
}
