package tapl.language.fullomega

import tapl.common._
import tapl.component._

trait Print[A[-R, E, -T, -K], V1, V2] extends Alg[Exp3[A, V1, V2], String, V1, V2]
  with typed.Print[({type l[-X, Y, -Z] = A[X, Y, Z, V2]})#l, V1]
  with extension.Print[({type l[-X, Y, -Z] = A[X, Y, Z, V2]})#l, V1]
  with pack.Print[({type l[-X, Y, -Z] = A[X, Y, Z, V2]})#l, V1]
  with ref.Print[({type l[-X, Y] = A[X, Y, V1, V2]})#l] {

  def printK(t: V2): String

  override def tmTAbs(x: String, k: V2, e: Exp3[A, V1, V2]): String =
    "\\(" + x + ":" + printK(k) + ")." + apply(e)

  override def tmTApp(e: Exp3[A, V1, V2], t: V1): String =
    apply(e) + " [" + printT(t) + "]"
}

object Print extends Print[Alg, Exp2[TAlg, Exp[KAlg]], Exp[KAlg]] with Impl[String] {
  override def printT(t: Exp2[TAlg, Exp[KAlg]]): String = t(TPrint)

  override def printK(t: Exp[KAlg]): String = t(KPrint)
}

trait TPrint[A[-F, T, -K], V] extends TAlg[Exp2[A, V], String, V]
  with typed.TPrint[({type l[-X, Y] = A[X, Y, V]})#l]
  with extension.TPrint[({type l[-X, Y] = A[X, Y, V]})#l]
  with ref.TPrint[({type l[-X, Y] = A[X, Y, V]})#l] {

  def printK(t: V): String

  override def tyAll(x: String, k: V, t: Exp2[A, V]): String =
    "All " + x + ":" + printK(k) + "." + apply(t)

  override def tySome(x: String, k: V, t: Exp2[A, V]): String =
    "{Some " + x + ":" + printK(k) + "," + apply(t) + "}"

  override def tyAbs(x: String, k: V, t: Exp2[A, V]): String =
    "\\(" + x + ":" + printK(k) + ")." + apply(t)

  override def tyApp(t1: Exp2[A, V], t2: Exp2[A, V]): String =
    "(" + apply(t1) + " " + apply(t2) + ")"
}

object TPrint extends TPrint[TAlg, Exp[KAlg]] with TImpl[String] {
  override def printK(t: Exp[KAlg]): String = t(KPrint)
}

trait KPrint[A[-X, Y]] extends KAlg[Exp[A], String] {
  override def knStar(): String = "Star"

  override def knArr(k1: Exp[A], k2: Exp[A]): String = apply(k1) + "=>" + apply(k2)
}

object KPrint extends KPrint[KAlg] with KImpl[String]
