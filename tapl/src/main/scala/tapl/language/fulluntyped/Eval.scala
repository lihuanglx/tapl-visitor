package tapl.language.fulluntyped

import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with arith.Eval[A] with untyped.Eval[A]
  with floatstring.Eval[A] with let.Eval[A] with record.Eval[A]

object Eval extends Eval[Alg] with Impl[Exp[Alg]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsVal

  override val subst: (String, Exp[Alg]) => Alg[Exp[Alg], Exp[Alg]] = (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with arith.IsVal[A] with untyped.IsVal[A]
  with floatstring.IsVal[A] with record.IsVal[A]

object IsVal extends IsVal[Alg] with Impl[Boolean]

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with untyped.Subst[A]

class SubstImpl(_x: String, _e: Exp[Alg]) extends Subst[Alg] with Impl[Exp[Alg]] {
  override val x: String = _x
  override val e: Exp[Alg] = _e
}
