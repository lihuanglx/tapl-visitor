package tapl.common

abstract class Context[A[-R, _]] {
  def lookup(x: String): Exp[A]
}
