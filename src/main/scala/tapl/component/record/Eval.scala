package tapl.component.record

import tapl.common.{EvalAuxiliary, Exp}

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  //def matcher[E]: Matcher

  override def TmRecord(l: List[(String, Exp[A])]): M[Exp[A]] = ???

  // todo
  override def TmProj(e: Exp[A], x: String): M[Exp[A]] =
    if (e(isVal)) {
      ???
    } else for {
      _e <- apply(e)
    } yield f.TmProj(_e, x)
}
