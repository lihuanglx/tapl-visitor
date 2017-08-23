package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.Term.{Query, Transform}
import tapl.language.fullerror.Term.Factory._

trait Eval[A[-R, E, -F] <: Term[R, E, F], V] extends Term[Exp2[A, V], Exp2[A, V], V]
  with bot.Eval[A, V] with typedbool.Eval[A[-?, ?, V]] {

  override def tmError(): Exp2[A, V] = TmError[A, V]()

  override def tmTry(e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] = e1 match {
    case TmError() => e2
    case _ => if (e1(isVal)) e1 else TmTry[A, V](apply(e1), e2)
  }

  override def tmApp(e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] = (e1, e2) match {
    case (TmError(), _) => e1
    case (_, TmError()) => e2
    case _ => super.tmApp(e1, e2)
  }

  override def tmIf(e1: Exp2[A, V], e2: Exp2[A, V], e3: Exp2[A, V]): Exp2[A, V] = e1 match {
    case TmError() => TmError[A, V]()
    case _ => super.tmIf(e1, e2, e3)
  }
}

object Eval extends Eval[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val isVal: Term[Exp2[Term, Exp[Type]], Boolean, Exp[Type]] = IsVal

  override def subst(m: Map[String, Exp2[Term, Exp[Type]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with bot.IsVal[A, V] with typedbool.IsVal[A[-?, ?, V]] {

  override def tmError(): Boolean = true
}

object IsVal extends IsVal[Term, Exp[Type]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Term[R, E, F], V] extends Transform[A, V] with bot.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Term, Exp[Type]]]) extends Subst[Term, Exp[Type]] with Impl[Exp2[Term, Exp[Type]]] {
  override val m: Map[String, Exp2[Term, Exp[Type]]] = mp
}
