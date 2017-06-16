package tapl.component.pack

import tapl.common._

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V] with PrintT[V] {
  override def tmPack(t1: V, e: Exp2[A, V], t2: V): String =
    "{*" + printT(t1) + "," + apply(e) + "} as " + printT(t2)

  override def tmUnpack(tx: String, x: String, e1: Exp2[A, V], e2: Exp2[A, V]): String =
    "let {" + tx + "," + x + "} = " + apply(e1) + " in " + apply(e2)
}
