package tapl.component.pack

import tapl.common._

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V] with PrintT[V] {
  override def tmPack(t1: V, e: TExp[A, V], t2: V): String =
    "{*" + printT(t1) + "," + apply(e) + "} as " + printT(t2)

  override def tmUnpack(tx: String, x: String, e1: TExp[A, V], e2: TExp[A, V]): String =
    "let {" + tx + "," + x + "} = " + apply(e1) + " in " + apply(e2)
}
