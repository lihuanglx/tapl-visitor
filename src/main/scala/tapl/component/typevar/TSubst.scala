package tapl.component.typevar

import tapl.common._
import tapl.component.top
import tapl.component.top.CTyTop

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with SubstAux[A] {
  override def TyVar(x: String): Exp[A] = if (x == this.x) this.e else CTyVar[A](x)
}
