package tapl.common

trait Exp[-A[-R, _]] {
  def apply[E](alg: A[Exp[A], E]): E
}

/*
trait Visitor {
  type R
  type A
}

trait ExpT {
  type A <: Visitor
  type Vis[X,Y] = A {type R = X; type A = Y}
  /*
  type B[-R,X] = A[R,X]
  type ExpA = ExpT {type A[-R,X] = B[R,X]}
  */

  def apply[E](alg: Vis[ExpT, E]): E
}
*/
