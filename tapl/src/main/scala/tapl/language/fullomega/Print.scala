package tapl.language.fullomega

import tapl.common._
import tapl.component._

trait Print[A[-R, E, -T, -K], V1, V2] extends Alg[Exp3[A, V1, V2], String, V1, V2]
  with typed.Print[({type l[-X, Y, -Z] = A[X, Y, Z, V2]})#l, V1]
  with extension.Print[({type l[-X, Y, -Z] = A[X, Y, Z, V2]})#l, V1]
  with pack.Print[({type l[-X, Y, -Z] = A[X, Y, Z, V2]})#l, V1]
  with ref.Print[({type l[-X, Y] = A[X, Y, V1, V2]})#l] {


}

/*
object Print extends Print[Alg, Exp[TAlg]] with Impl[String] {
  override def printT(t: Exp[TAlg]): String = t(TPrint)
}


trait TPrint[A[-F, T]] extends TAlg[Exp[A], String]
  with typed.TPrint[A] with extension.TPrint[A] with typevar.TPrint[A] {

  override def tyAll(x: String, t: Exp[A]): String = "All " + x + "." + apply(t)

  override def tySome(x: String, t: Exp[A]): String = "{Some " + x + "," + apply(t) + "}"
}

object TPrint extends TPrint[TAlg] with TImpl[String]
*/
