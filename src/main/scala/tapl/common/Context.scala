package tapl.common

class Context[K, V](m: Map[K, V]) {
  def +(b: (K, V)): Context[K, V] = new Context(m + b)

  def apply(k: K): V = m(k)
}

object Context {
  def empty[K, V](): Context[K, V] = new Context[K, V](Map())
}
