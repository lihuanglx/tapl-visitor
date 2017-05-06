package tapl.component.varapp

import tapl.common.{EvalAuxiliary, Exp, Util}
import tapl.component.varapp.Factory._

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  val subst: (String, Exp[A]) => A[Exp[A], Exp[A]]
  val isFuncVal: A[Exp[A], Option[(String, Exp[A])]]

  override def TmVar(x: String): M[Exp[A]] = m.point(CVar[A](x))

  override def TmApp(e1: Exp[A], e2: Exp[A]): M[Exp[A]] = {
    if (e1(isVal))
      if (e2(isVal)) {
        val (x, body) = e1(isFuncVal).getOrElse(Util.typeError())
        m.point(subst(x, e2)(body))
      } else for {
        _e2 <- apply(e2)
      } yield CApp(e1, _e2)
    else for {
      _e1 <- apply(e1)
    } yield CApp(_e1, e2)
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmVar(x: String): Boolean = true
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] {
  val x: String
  val e: Exp[A]

  override def TmVar(x: String): Exp[A] = if (x == this.x) e else CVar[A](x)
}