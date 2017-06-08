package tapl.component.simple

import tapl.common._
import tapl.component._
import tapl.language.tyarith

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V] with typed.Print[A, V]
  with tyarith.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Print[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typedrecord.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with PrintT[V] {

  override def TmUnit(): String = "unit"

  override def TmAscribe(e: E3[A, V], t: V): String = "(" + apply(e) + ") as " + printT(t)

  override def TmFix(e: E3[A, V]): String = "fix (" + apply(e) + ")"

  override def TmInert(t: V): String = "inert [" + t + "]"
}

trait TPrint[A[-R, _]] extends TAlg[Exp[A], String] with typed.TPrint[A]
  with tyarith.TPrint[A] with typedrecord.TPrint[A] {

  override def TyUnit(): String = "Unit"

  override def TyString(): String = "String"

  override def TyFloat(): String = "Float"
}
