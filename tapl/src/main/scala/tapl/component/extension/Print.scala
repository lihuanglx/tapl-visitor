package tapl.component.extension

import tapl.common._
import tapl.component._
import tapl.language.tyarith

trait Print[A[-R, E, -F], V] extends Term[Exp2[A, V], String, V]
  with tyarith.Print[A[-?, ?, V]] with floatstring.Print[A[-?, ?, V]] with let.Print[A[-?, ?, V]]
  with typedrecord.Print[A[-?, ?, V]] with unit.Print[A[-?, ?, V]] with PrintT[V] {

  override def tmAscribe(e: Exp2[A, V], t: V): String = "(" + apply(e) + ") as " + printT(t)

  override def tmFix(e: Exp2[A, V]): String = "fix (" + apply(e) + ")"
}

trait TPrint[A[-R, _]] extends Type[Exp[A], String] with tyarith.TPrint[A]
  with typedrecord.TPrint[A] with unit.TPrint[A] {

  override def tyString(): String = "String"

  override def tyFloat(): String = "Float"
}
