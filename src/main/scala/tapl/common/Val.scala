package tapl.common

import scalaz.Monad

object Val {

  trait Val

  case class BoolVal(b: Boolean) extends Val

  case class IntVal(x: Int) extends Val

  implicit def boolToVal(b: Boolean): Val = BoolVal(b)

  implicit def intToVal(x: Int): Val = IntVal(x)

  // todo: add constraint
  def intVal[M[_] : Monad](v: Val): M[Int] = v match {
    case IntVal(x) => implicitly[Monad[M]].point(x)
    case _ => Util.typeError()
  }

  // todo: add constraint
  def boolVal[M[_] : Monad](v: Val): M[Boolean] = v match {
    case BoolVal(b) => implicitly[Monad[M]].point(b)
    case _ => Util.typeError()
  }
}

