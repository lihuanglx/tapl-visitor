package tapl.language.arith

import tapl.common.Exp
import tapl.common.Val._
import tapl.component._
import tapl.language.arith.Syntax._

import scalaz.Monad

trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Val]] with bool.Eval[A, M] with nat.Eval[A, M]

trait EvalM[M[_]] extends Eval[Alg, M] {
  override def visit(e: Exp[Alg]): M[Val] = e(this)
}
