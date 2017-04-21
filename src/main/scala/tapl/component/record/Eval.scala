package tapl.component.record

import tapl.common.{EvalAuxiliary, Exp}
import tapl.common.Util.typeError

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  val f: A[Exp[A], Exp[A]]

  override def TmRecord(l: List[(String, Exp[A])]): M[Exp[A]] = ???

  // todo
  override def TmProj(e: Exp[A], x: String): M[Exp[A]] = ???
}
