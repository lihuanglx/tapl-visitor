
package object gems {

  trait SExp[-A[-R, _], -B[-F, _]] {
    def apply[E](alg: A[SExp[B, B], E]): E
  }

  type Exp[-A[-R, _]] = SExp[A, A]

  type SExp2[-A[-R, E, -F], -B[-R, E], +V] = SExp[({type l[-X, Y] = A[X, Y, V]})#l, B]

  type Exp2[-A[-R, E, -F], +V] = SExp2[A, ({type l[-X, Y] = A[X, Y, V]})#l, V]

  trait Default[T] {
    def default: T
  }

}
