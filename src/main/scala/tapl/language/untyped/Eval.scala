package tapl.language.untyped

import tapl.common.Exp
import tapl.component._
import tapl.language.untyped.Factory._
import tapl.common.Util._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with varapp.Eval[A] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs(x, e)

  override def TmApp(e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      if (e2(isVal)) e1 match {
        case CAbs(x, body) => body(subst(x, e2))
        case _ => typeError()
      } else {
        CApp(e1, apply(e2))
      }
    } else {
      CApp(apply(e1), e2)
    }
}

trait EvalM extends Eval[Alg] with Impl[Exp[Alg]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl

  override val subst: (String, Exp[Alg]) => Alg[Exp[Alg], Exp[Alg]] = (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with varapp.IsVal[A] {
  override def TmAbs(x: String, e: Exp[A]) = true
}

object IsValImpl extends IsVal[Alg] with Impl[Boolean]

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with varapp.Subst[A] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs[A](x, if (this.x == x) e else apply(e))
}

class SubstImpl(_x: String, _e: Exp[Alg]) extends Subst[Alg] with Impl[Exp[Alg]] {
  override val x: String = _x
  override val e: Exp[Alg] = _e
}