package tapl.component.varapp

import tapl.common.{Context, EvalAuxiliary, Exp, Util}

import scalaz.Scalaz._
import scalaz._

// todo: check
trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  implicit val m: MonadState[M, Context[A]]

  val f: Alg[Exp[A], Exp[A]]

  val subst: (String, Exp[A]) => Alg[Exp[A], Exp[A]]
  val isFuncVal: A[Exp[A], Option[(String, Exp[A])]]

  override def TmVar(x: String): M[Exp[A]] = for {
    c <- m.get
  } yield c.lookup(x)

  override def TmApp(e1: Exp[A], e2: Exp[A]): M[Exp[A]] = {
    if (e1(isVal))
      if (e2(isVal)) {
        val (x, body) = e1(isFuncVal).getOrElse(Util.typeError())
        m.point(subst(x, e1)(body))
      } else for {
        _e2 <- apply(e2)
      } yield f.TmApp(e1, _e2)
    else for {
      _e1 <- apply(e1)
    } yield f.TmApp(_e1, e2)
  }
}
