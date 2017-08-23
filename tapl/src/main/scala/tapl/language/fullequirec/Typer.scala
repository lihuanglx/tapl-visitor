package tapl.language.fullequirec

import tapl.common._
import tapl.language.fullequirec.Type.Factory._
import tapl.component.{extension, variant}
import tapl.language.equirec

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]]
  with equirec.Typer[A, B] with extension.Typer[A, B] with variant.Typer[A, B] {

  private def unwrap(t: Exp[B]): Exp[B] = t match {
    case TyRec(x, r) => unwrap(r(subst(x, t)))
    case _ => t
  }

  override def tmTag(x: String, e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = super.tmTag(x, e, unwrap(t))

  override def tmCase(e: Exp2[A, Exp[B]], l: List[(String, String, Exp2[A, Exp[B]])]): CtxTo[B] = c =>
    unwrap(apply(e)(c)) match {
      case TyVariant(l2) =>
        l.map({ case (n, v, b) =>
          val t = l2.find(_._1 == n).getOrElse(typeError())._2
          apply(b)(c + (v, t))
        }).reduce((x, y) => if (tEquals(x)(y)) x else typeError())
      case _ => typeError()
    }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean = _ (TEquals)(Set.empty)

  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  with equirec.TEquals[A] {

  type T = Set[(Exp[A], Exp[A])] => Exp[A] => Boolean

  private def defaultEq(t: Exp[A], f: T): T =
    c => u => c((t, u)) || (u match {
      case TyRec(_, _) => apply(u)(c)(t)
      case _ => f(c)(u)
    })

  override def tyUnit(): T = defaultEq(TyUnit[A](), _ => {
    case TyUnit() => true
    case _ => false
  })

  override def tyFloat(): T = defaultEq(TyFloat[A](), _ => {
    case TyFloat() => true
    case _ => false
  })

  override def tyNat(): T = defaultEq(TyNat[A](), _ => {
    case TyNat() => true
    case _ => false
  })

  override def tyBool(): T = defaultEq(TyBool[A](), _ => {
    case TyBool() => true
    case _ => false
  })

  // todo
  override def tyRecord(l: List[(String, Exp[A])]): T = defaultEq(TyRecord(l), c => {
    case TyRecord(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(c)(t2) && b
        case _ => false
      }
    })
    case _ => false
  })

  override def tyVariant(l: List[(String, Exp[A])]): T = defaultEq(TyVariant(l), c => {
    case TyVariant(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(c)(t2) && b
        case _ => false
      }
    })
    case _ => false
  })

  override def tyString(): T = defaultEq(TyString[A](), _ => {
    case TyString() => true
    case _ => false
  })
}

object TEquals extends TEquals[Type] with TImpl[Set[(Exp[Type], Exp[Type])] => Exp[Type] => Boolean] {
  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with equirec.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[Type]]) extends TSubst[Type] with TImpl[Exp[Type]] {
  override val m: Map[String, Exp[Type]] = mp
}
