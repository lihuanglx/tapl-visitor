package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[B]]]
  with typedbool.Typer[A, B, M] with typednat.Typer[A, B, M]

trait TyperM[M[_]] extends Typer[Alg, TAlg, M] with Impl[M[Exp[TAlg]]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] = new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean] {}
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typedbool.TEquals[A] with typednat.TEquals[A]
