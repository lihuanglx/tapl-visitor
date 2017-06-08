package play.examples

object Common {

  trait Exp[-A[-R, _]] {
    def apply[E](alg: A[Exp[A], E]): E
  }

  trait Default[T] {
    val default: T
  }

  type TExp[-A[-R, E, -F], +V] = Exp[({type lam[-X, Y] = A[X, Y, V]})#lam]
}
