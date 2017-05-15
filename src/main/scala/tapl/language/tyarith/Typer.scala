package tapl.language.tyarith

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.{typedbool, typednat}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp[A], Type[B]] with typedbool.Typer[A, B] with typednat.Typer[A, B]

trait TyperM extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typedbool.TEquals[A] with typednat.TEquals[A]
