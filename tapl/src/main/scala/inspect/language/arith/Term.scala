package inspect.language.arith

import inspect.common._
import inspect.component._
import macros.Lang

@Lang("arith")
trait Term[-R, E] extends bool.Term[R, E] with nat.Term[R, E]

trait Impl[T] extends Term[Exp[Term], T] with Term.Inspect[Term] {
  override def apply(e: Exp[Term]): T = e(this)

  override def inspectArith[B[-R, _]](e: SExp[Term, B]): Option[SExp[Term, B]] = Some(e)
}
