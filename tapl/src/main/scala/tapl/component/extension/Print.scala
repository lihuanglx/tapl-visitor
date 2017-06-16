package tapl.component.extension

import tapl.common._
import tapl.component._
import tapl.language.tyarith

trait Print[A[-R, E, -F], V] extends Alg[Exp2[A, V], String, V]
  with tyarith.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typedrecord.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with unit.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with PrintT[V] {

  override def tmAscribe(e: Exp2[A, V], t: V): String = "(" + apply(e) + ") as " + printT(t)

  override def tmFix(e: Exp2[A, V]): String = "fix (" + apply(e) + ")"
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with tyarith.TPrint[A]
  with typedrecord.TPrint[A] with unit.TPrint[A] {

  override def tyString(): String = "String"

  override def tyFloat(): String = "Float"
}
