package tapl.language.untyped

import tapl.common._
import tapl.component._
import tapl.language.untyped.Term.{Query, Transform}
import tapl.language.untyped.Term.Factory._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]]
  with varapp.Eval[A] with IIsVal[A] with ISubst[A] {

  override def tmAbs(x: String, e: Exp[A]): Exp[A] = TmAbs(x, e)

  override def tmApp(e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      if (e2(isVal)) e1 match {
        case TmAbs(x, body) => body(subst(x, e2))
        case _ => typeError()
      } else {
        TmApp(e1, apply(e2))
      }
    } else {
      TmApp(apply(e1), e2)
    }
}

object Eval extends Eval[Term] with Impl[Exp[Term]] {
  override val isVal: Term[Exp[Term], Boolean] = IsVal

  override def subst(m: Map[String, Exp[Term]]): Term[Exp[Term], Exp[Term]] = new SubstImpl(m)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with varapp.IsVal[A] {
  override def tmAbs(x: String, e: Exp[A]) = true
}

object IsVal extends IsVal[Term] with Impl[Boolean]

trait Subst[A[-X, Y] <: Term[X, Y]] extends Transform[A] with varapp.Subst[A] {
  override def tmAbs(x: String, e: Exp[A]): Exp[A] = TmAbs[A](x, if (m.contains(x)) e else apply(e))
}

class SubstImpl(mp: Map[String, Exp[Term]]) extends Subst[Term] with Impl[Exp[Term]] {
  override val m: Map[String, Exp[Term]] = mp
}
