package tapl.component.varapp

import tapl.common._
import tapl.component.varapp.Alg._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with IIsVal[A] {
  override def tmVar(x: String): Exp[A] = TmVar[A](x)

  override def tmSeq(es: List[Exp[A]]): Exp[A] = es match {
    case a :: as =>
      if (a(isVal)) if (as.isEmpty) a else TmSeq[A](as)
      else TmSeq[A](apply(a) :: as)
    case Nil => sys.error("Empty sequence")
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  // todo
  override def tmVar(x: String): Boolean = true
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def tmVar(x: String): Exp[A] =
    if (m.contains(x)) m(x) else TmVar[A](x)
}
