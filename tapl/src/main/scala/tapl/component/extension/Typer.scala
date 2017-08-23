package tapl.component.extension

import tapl.common._
import tapl.component._
import tapl.language.tyarith
import tapl.component.extension.Type.Factory._
import tapl.component.typed.Type.Factory.TyArr
import tapl.component.top.Type.Factory.TyTop

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with ITEq[B]
    with tyarith.Term.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]]
    with typedrecord.Term.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]]
    with let.Typer[A[-?, ?, Exp[B]], B] {

  override def propagate(c: Ctx[String, Exp[B]]): tyarith.Term[Exp2[A, Exp[B]], Exp[B]]
    with typedrecord.Term[Exp2[A, Exp[B]], Exp[B]] =
    new tyarith.Typer[A[-?, ?, Exp[B]], B] with typedrecord.Typer[A[-?, ?, Exp[B]], B] {

      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] =
        Typer.this.apply(e)(c)

      override val tEquals: (Exp[B]) => (Exp[B]) => Boolean = Typer.this.tEquals
    }

  override def tmUnit(): CtxTo[B] = TyUnit[B]()

  override def tmAscribe(e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = c =>
    if (tEquals(apply(e)(c))(t)) t else typeError()

  override def tmFix(e: Exp2[A, Exp[B]]): CtxTo[B] = c => {
    apply(e)(c) match {
      case TyArr(t1, t2) if tEquals(t1)(t2) => t1
      case _ => typeError()
    }
  }

  override def tmFloat(d: Double): CtxTo[B] = TyFloat[B]()

  override def tmString(s: String): CtxTo[B] = TyString[B]()

  override def tmTimes(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c => {
    (apply(e1)(c), apply(e2)(c)) match {
      case (TyFloat(), TyFloat()) => TyFloat[B]()
      case _ => typeError()
    }
  }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with tyarith.TEquals[A] with typedrecord.TEquals[A] with unit.TEquals[A] {

  override def tyString(): (Exp[A]) => Boolean = {
    case TyString() => true
    case _ => false
  }

  override def tyFloat(): (Exp[A]) => Boolean = {
    case TyFloat() => true
    case _ => false
  }
}

trait Typer2[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Typer[A, B] with IJoin[B] with ISubtypeOf[B] {

  override def propagate(c: Ctx[String, Exp[B]]) =
    new tyarith.Typer2[A[-?, ?, Exp[B]], B] with typedrecord.Typer[A[-?, ?, Exp[B]], B] {

      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] =
        Typer2.this.apply(e)(c)

      override val join: B[Exp[B], (Exp[B]) => Exp[B]] = Typer2.this.join

      override val tEquals: (Exp[B]) => (Exp[B]) => Boolean = Typer2.this.tEquals

      override val subtypeOf: B[Exp[B], (Exp[B]) => Boolean] = Typer2.this.subtypeOf
    }

  override def tmAscribe(e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = c =>
    if (apply(e)(c)(subtypeOf)(t)) t else typeError()

  override def tmFix(e: Exp2[A, Exp[B]]): CtxTo[B] = c => {
    apply(e)(c) match {
      case TyArr(t1, t2) if t2(subtypeOf)(t1) => t2
      case _ => typeError()
    }
  }
}

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with tyarith.SubtypeOf[A] with typedrecord.SubtypeOf[A] with unit.SubtypeOf[A] {

  override def tyFloat(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyFloat() => true
    case _ => false
  }

  override def tyString(): Exp[A] => Boolean = {
    case TyTop() => true
    case TyString() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: Type[X, Y] with top.Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]]
  with tyarith.Join[A] with typedrecord.Join[A] with unit.Join[A] {

  override def tyFloat(): Exp[A] => Exp[A] = directJoin(TyFloat[A](), _).getOrElse(TyTop[A]())

  override def tyString(): Exp[A] => Exp[A] = directJoin(TyString[A](), _).getOrElse(TyTop[A]())
}

