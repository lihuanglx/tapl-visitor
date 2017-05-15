package tapl.common

object Util {
  def typeError(msg: String = "Type error!"): Nothing = sys.error(msg)

  type E3[-A[-R, E, -F], +V] = Exp[({type lam[-X, Y] = A[X, Y, V]})#lam]

  type Type[A[-X, Y]] = Context[String, Exp[A]] => Exp[A]
}
