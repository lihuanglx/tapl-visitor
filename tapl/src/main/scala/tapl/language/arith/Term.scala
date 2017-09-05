package tapl.language.arith

import tapl.common._
import tapl.component._
import gems.Language

@Language
trait Term[-R, E] extends bool.Term[R, E] with nat.Term[R, E]

trait Impl[T] extends Term[Exp[Term], T] {
  override def apply(e: Exp[Term]): T = e(this)
}
