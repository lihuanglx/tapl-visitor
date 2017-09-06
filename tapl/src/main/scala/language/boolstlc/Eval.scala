package language
package boolstlc

import gems._

trait Eval[A[-R, E, -F] <: Term[R, E, F], T] extends Term[Exp2[A, T], Exp2[A, T], T]
  with bool.Eval[A[-?, ?, T]] with stlc.Eval[A, T] with Term.AllChains[A, T]

trait IsVal[A[-R, E, -F], V] extends Term.Query[Exp2[A, V], Boolean, V]
  with bool.IsVal[A[-?, ?, V]] with stlc.IsVal[A, V]

trait Subst[A[-R, E, -F] <: Term[R, E, F], T] extends Term.Transform[A, T] with stlc.Subst[A, T]

trait Print[A[-R, E, -F], T] extends Term[Exp2[A, T], String, T]
  with bool.Print[A[-?, ?, T]] with stlc.Print[A, T]

trait PrintT[A[-R, _]] extends Type[Exp[A], String]
  with bool.PrintT[A] with stlc.PrintT[A]
