package tapl.language.fulluntyped

import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}
import tapl.language.fulluntyped.Alg._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with arith.Eval[A] with untyped.Eval[A]
  with floatstring.Eval[A] with let.Eval[A] with record.Eval[A]

object Eval extends Eval[Alg] with Impl[Exp[Alg]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsVal

  override def subst(m: Map[String, Exp[Alg]]): Alg[Exp[Alg], Exp[Alg]] = new SubstImpl(m)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with arith.IsVal[A] with untyped.IsVal[A]
  with floatstring.IsVal[A] with record.IsVal[A]

object IsVal extends IsVal[Alg] with Impl[Boolean]

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with untyped.Subst[A]

class SubstImpl(mp: Map[String, Exp[Alg]]) extends Subst[Alg] with Impl[Exp[Alg]] {
  override val m: Map[String, Exp[Alg]] = mp
}
