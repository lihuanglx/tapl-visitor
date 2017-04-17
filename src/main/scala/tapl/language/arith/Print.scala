package tapl.language.arith

import tapl.common.Exp
import tapl.component._
import tapl.language.arith.Syntax._

trait Print[A[-R, _]] extends Alg[Exp[A], String] with bool.Print[A] with nat.Print[A]

object PrintImpl extends Print[Alg] {
  override def visit(e: Exp[Alg]): String = e(this)
}