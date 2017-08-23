package tapl.language.fulluntyped

import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}
import tapl.language.fulluntyped.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with arith.Eval[A] with untyped.Eval[A]
  with floatstring.Eval[A] with let.Eval[A] with record.Eval[A]

object Eval extends Eval[Term] with Impl[Exp[Term]] {
  override val isVal: Term[Exp[Term], Boolean] = IsVal

  override def subst(m: Map[String, Exp[Term]]): Term[Exp[Term], Exp[Term]] = new SubstImpl(m)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with arith.IsVal[A] with untyped.IsVal[A]
  with floatstring.IsVal[A] with record.IsVal[A]

object IsVal extends IsVal[Term] with Impl[Boolean]

trait Subst[A[-X, Y] <: Term[X, Y]] extends Transform[A] with untyped.Subst[A]

class SubstImpl(mp: Map[String, Exp[Term]]) extends Subst[Term] with Impl[Exp[Term]] {
  override val m: Map[String, Exp[Term]] = mp
}
