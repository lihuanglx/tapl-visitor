package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with bot.Typer[A, B]
    with typedbool.Typer2[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def TmError(): Type[B] = CTyBot[B]()

  override def TmTry(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c => {
    val t1 = apply(e1)(c)
    val t2 = apply(e2)(c)
    t1(join)(t2)
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

  override val subtypeOf: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] = SubtypeOf

  override val join: TAlg[Exp[TAlg], (Exp[TAlg]) => Exp[TAlg]] = Join
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with bot.TEquals[A] with typedbool.TEquals[A]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with bot.SubtypeOf[A] with typedbool.SubtypeOf[A]

object SubtypeOf extends SubtypeOf[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with bot.Join[A] with typedbool.Join[A]

object Join extends Join[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val meet: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Meet
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with bot.Meet[A] {
  override def tyBool(): Exp[A] => Exp[A] = directMeet(TyBool[A](), _).getOrElse(CTyBot[A]())

  override def TyId(x: String): Exp[A] => Exp[A] = directMeet(CTyId[A](x), _).getOrElse(CTyBot[A]())
}

object Meet extends Meet[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val join: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Join
}
