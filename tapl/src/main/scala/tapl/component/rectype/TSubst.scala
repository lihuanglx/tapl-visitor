package tapl.component.rectype

import tapl.common._
import tapl.component.rectype.TAlg.Factory._

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with SubstAux[A] {
  override def tyRec(x: String, t: Exp[A]): Exp[A] = TyRec[A](x, if (m.contains(x)) t else apply(t))
}
