package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], Type[B], Exp[B]] with bot.Typer[A, B]
    with typedbool.Alg.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]] with IJoin[B] {

  override def go(c: Ctx[String, Exp[B]]): typedbool.Alg[Exp2[A, Exp[B]], Exp[B]] =
    new typedbool.Typer2[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {
      override def apply(e: Exp[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam]): Exp[B] =
        Typer.this.apply(e)(c)

      override val tEquals: (Exp[B]) => (Exp[B]) => Boolean = Typer.this.tEquals

      override val join: B[Exp[B], (Exp[B]) => Exp[B]] = Typer.this.join

      override val subtypeOf: B[Exp[B], (Exp[B]) => Boolean] = Typer.this.subtypeOf
    }

  override def tmError(): Type[B] = TyBot[B]()

  override def tmTry(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): Type[B] = c => {
    val t1 = apply(e1)(c)
    val t2 = apply(e2)(c)
    t1(join)(t2)
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean =
    _ (new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean])

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
  override def tyBool(): Exp[A] => Exp[A] = directMeet(TyBool[A](), _).getOrElse(TyBot[A]())

  override def tyVar(x: String): Exp[A] => Exp[A] = directMeet(TyVar[A](x), _).getOrElse(TyBot[A]())
}

object Meet extends Meet[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val join: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Join
}
