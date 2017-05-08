package tapl.language.untyped

import tapl.common.Exp
import tapl.component.varapp

trait Alg[-R, E] extends varapp.Alg[R, E] {
  def TmAbs(x: String, e: R): E
}

trait Factory extends varapp.Factory {

  case class CAbs[A[-X, Y] <: Alg[X, Y]](x: String, e: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmAbs(x, e)
  }

}

object Factory extends Factory

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}