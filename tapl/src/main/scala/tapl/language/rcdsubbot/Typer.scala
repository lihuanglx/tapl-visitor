package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], Type[B], Exp[B]] with bot.Typer[A, B]
    with typedrecord.Alg.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]] {

  override def propagate(c: Ctx[String, Exp[B]]): typedrecord.Alg[Exp2[A, Exp[B]], Exp[B]] =
    new typedrecord.Typer2[A[-?, ?, Exp[B]], B] {
      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] = Typer.this.apply(e)(c)
    }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean =
    _ (new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean])

  override val subtypeOf: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new SubtypeOf[TAlg] with TImpl[Exp[TAlg] => Boolean]
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]]
  extends TAlg[Exp[A], Exp[A] => Boolean] with bot.TEquals[A] with typedrecord.TEquals[A]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with bot.SubtypeOf[A] with typedrecord.SubtypeOf[A]
