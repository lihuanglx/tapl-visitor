package tapl.language.fulluntyped

import macros.Language
import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

@Language
trait Term[-R, E] extends arith.Term[R, E] with untyped.Term[R, E]
  with record.Term[R, E] with floatstring.Term[R, E] with let.Term[R, E]

trait Impl[T] extends Term[Exp[Term], T] {
  override def apply(e: Exp[Term]): T = e(this)
}
