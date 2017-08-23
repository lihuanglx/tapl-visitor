package tapl.component.rectype

import tapl.common._
import tapl.component.rectype.Type.Factory._

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with SubstAux[A] {
  override def tyRec(x: String, t: Exp[A]): Exp[A] = TyRec[A](x, if (m.contains(x)) t else apply(t))
}
