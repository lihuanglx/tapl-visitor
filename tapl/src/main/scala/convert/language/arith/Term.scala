package convert.language.arith

import convert.common._
import convert.component._
import macros.Lang

@Lang("arith")
trait Term[-R, E] extends bool.Term[R, E] with nat.Term[R, E]

trait Impl[T] extends Term[Exp[Term], T] with Term.Convert[Term] {
  override def apply(e: Exp[Term]): T = e(this)

  override def convertArith[B[-R, _]](e: SExp[Term, B]): Option[SExp[Term, B]] = Some(e)
}
