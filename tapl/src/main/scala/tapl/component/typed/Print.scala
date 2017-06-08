package tapl.component.typed

import tapl.common._
import tapl.component.varapp

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V]
  with varapp.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with PrintT[V] {

  override def tmAbs(x: String, t: V, e: TExp[A, V]): String = "\\(" + x + ":" + printT(t) + ")." + apply(e)
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] {
  override def tyArr(t1: Exp[A], t2: Exp[A]): String = "(" + apply(t1) + ")->" + apply(t2)

  override def tyId(x: String): String = x
}
