package tapl.language.tyarith

import tapl.common._
import tapl.component.{top, typedbool, typednat}

trait Typer[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp[A], Exp[B]] with typedbool.Typer[A, B] with typednat.Typer[A, B]

object Typer extends Typer[Term, Type] with Impl[Exp[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean =
    _ (new TEquals[Type] with TImpl[Exp[Type] => Boolean])
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typedbool.TEquals[A] with typednat.TEquals[A]

trait Typer2[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp[A], Exp[B]] with typedbool.Typer2[A, B] with typednat.Typer[A, B]

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typedbool.SubtypeOf[A] with typednat.SubtypeOf[A]

trait Join[A[-X, Y] <: Type[X, Y] with top.Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]]
  with typedbool.Join[A] with typednat.Join[A]
