package tapl.language.arith

import tapl.common._
import tapl.component._
import macros.Visitor

@Visitor
trait Alg[-R, E] extends bool.Alg[R, E] with nat.Alg[R, E]

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}
