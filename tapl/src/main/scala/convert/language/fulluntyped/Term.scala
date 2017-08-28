package convert.language.fulluntyped

import macros.Lang
import convert.common._
import convert.component.{floatstring, let, record}
import convert.language.{arith, untyped}

@Lang("fulluntyped")
trait Term[-R, E] extends arith.Term[R, E] with untyped.Term[R, E]
  with record.Term[R, E] with floatstring.Term[R, E] with let.Term[R, E]

trait Impl[T] extends Term[Exp[Term], T] with Term.Convert[Term] {
  override def apply(e: Exp[Term]): T = e(this)

  override def convertFulluntyped[B[-R, _]](e: SExp[Term, B]): Option[SExp[Term, B]] = Some(e)
}
