package tapl.language.arith

import tapl.common.Exp
import tapl.common.Value._
import tapl.component._
import tapl.language.arith.Syntax._

import scalaz.Monad

trait Eval[A[-R, _], M[_] <: Monad[M]] extends Alg[Exp[A], M[Value]] with bool.Eval[A, M] with nat.Eval[A, M]

trait EvalM[M[_] <: Monad[M]] extends Eval[Alg, M] {
  override def visit(e: Exp[Alg]): M[Value] = e(this)
}
