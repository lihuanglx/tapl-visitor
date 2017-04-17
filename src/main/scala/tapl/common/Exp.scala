package tapl.common

trait Exp[-A[-R, _]] {
  def apply[E](alg: A[Exp[A], E]): E
}
