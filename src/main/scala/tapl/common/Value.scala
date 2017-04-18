package tapl.common

object Value {

  trait Value

  case class BoolVal(b: Boolean) extends Value

  case class IntVal(x: Int) extends Value

  implicit def boolToVal(b: Boolean): Value = BoolVal(b)

  implicit def intToVal(x: Int): Value = IntVal(x)

  def intVal: Value => Int = {
    case IntVal(x) => x
    case _ => Util.typeError()
  }

  def boolVal: Value => Boolean = {
    case BoolVal(b) => b
    case _ => Util.typeError()
  }
}

