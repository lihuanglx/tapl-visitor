package tapl.component.typevar

import tapl.common._
import tapl.component.typevar.TAlg.Transform
import tapl.component.typevar.TAlg.Factory._

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def tyVar(x: String): Exp[A] = if (m.contains(x)) m(x) else TyVar[A](x)
}
