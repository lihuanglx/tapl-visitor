package tapl.component.typed

import tapl.common.Exp
import tapl.common.Util.E3
import tapl.component.varapp
import tapl.common.PrintT

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V]
  with varapp.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with PrintT[V] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): String = "\\(" + x + ":" + printT(t) + ")." + apply(e)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): String = apply(t1) + "->" + apply(t2)
}
