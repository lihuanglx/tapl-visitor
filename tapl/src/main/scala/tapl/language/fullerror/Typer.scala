package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with bot.Typer[A, B]
    with typedbool.Term.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]] with IJoin[B] {

  override def propagate(c: Ctx[String, Exp[B]]): typedbool.Term[Exp2[A, Exp[B]], Exp[B]] =
    new typedbool.Typer2[A[-?, ?, Exp[B]], B] {
      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] = Typer.this.apply(e)(c)

      override val tEquals: (Exp[B]) => (Exp[B]) => Boolean = Typer.this.tEquals

      override val join: B[Exp[B], (Exp[B]) => Exp[B]] = Typer.this.join

      override val subtypeOf: B[Exp[B], (Exp[B]) => Boolean] = Typer.this.subtypeOf
    }

  override def tmError(): CtxTo[B] = TyBot[B]()

  override def tmTry(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c => {
    val t1 = apply(e1)(c)
    val t2 = apply(e2)(c)
    t1(join)(t2)
  }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean =
    _ (new TEquals[Type] with TImpl[Exp[Type] => Boolean])

  override val subtypeOf: Type[Exp[Type], (Exp[Type]) => Boolean] = SubtypeOf

  override val join: Type[Exp[Type], (Exp[Type]) => Exp[Type]] = Join
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with bot.TEquals[A] with typedbool.TEquals[A]

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with bot.SubtypeOf[A] with typedbool.SubtypeOf[A]

object SubtypeOf extends SubtypeOf[Type] with TImpl[Exp[Type] => Boolean]

trait Join[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]]
  with bot.Join[A] with typedbool.Join[A]

object Join extends Join[Type] with TImpl[Exp[Type] => Exp[Type]] {
  override val subtypeOf: Type[Exp[Type], Exp[Type] => Boolean] = SubtypeOf

  override val meet: Type[Exp[Type], Exp[Type] => Exp[Type]] = Meet
}

trait Meet[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with bot.Meet[A] {
  override def tyBool(): Exp[A] => Exp[A] = directMeet(TyBool[A](), _).getOrElse(TyBot[A]())

  override def tyVar(x: String): Exp[A] => Exp[A] = directMeet(TyVar[A](x), _).getOrElse(TyBot[A]())
}

object Meet extends Meet[Type] with TImpl[Exp[Type] => Exp[Type]] {
  override val subtypeOf: Type[Exp[Type], Exp[Type] => Boolean] = SubtypeOf

  override val join: Type[Exp[Type], Exp[Type] => Exp[Type]] = Join
}
