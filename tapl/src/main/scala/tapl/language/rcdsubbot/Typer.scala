package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with bot.Typer[A, B]
    with typedrecord.Term.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]] {

  override def propagate(c: Ctx[String, Exp[B]]): typedrecord.Term[Exp2[A, Exp[B]], Exp[B]] =
    new typedrecord.Typer2[A[-?, ?, Exp[B]], B] {
      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] = Typer.this.apply(e)(c)
    }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean =
    _ (new TEquals[Type] with TImpl[Exp[Type] => Boolean])

  override val subtypeOf: Type[Exp[Type], (Exp[Type]) => Boolean] =
    new SubtypeOf[Type] with TImpl[Exp[Type] => Boolean]
}

trait TEquals[A[-X, Y] <: Type[X, Y]]
  extends Type[Exp[A], Exp[A] => Boolean] with bot.TEquals[A] with typedrecord.TEquals[A]

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with bot.SubtypeOf[A] with typedrecord.SubtypeOf[A]
