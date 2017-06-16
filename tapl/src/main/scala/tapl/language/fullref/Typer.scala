package tapl.language.fullref

import tapl.common._
import tapl.component.{ref, variant}
import tapl.language.fullsub
import tapl.language.fullref.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], Ctx[Int, Exp[B]] => Ctx[String, Exp[B]] => Exp[B], Exp[B]]
    with IJoin[B] with ISubtypeOf[B] with ITEq[B]
    with fullsub.Alg.Lifter[Exp2[A, Exp[B]], Type[B], Exp[B], Ctx[Int, Exp[B]]]
    with variant.Alg.Lifter[Exp2[A, Exp[B]], Type[B], Exp[B], Ctx[Int, Exp[B]]]
    with ref.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def go(c: Ctx[Int, Exp[B]]) = new fullsub.Typer[A, B] with variant.Typer[A, B] {
    override def apply(e: Exp2[A, Exp[B]]): Type[B] = Typer.this.apply(e)(c)

    override val join: B[Exp[B], Exp[B] => Exp[B]] = Typer.this.join

    override val subtypeOf: B[Exp[B], Exp[B] => Boolean] = Typer.this.subtypeOf

    override val tEquals: Exp[B] => Exp[B] => Boolean = Typer.this.tEquals
  }

  override def tmDeRef(e: Exp2[A, Exp[B]]): (Ctx[Int, Exp[B]]) => Type[B] =
    c1 => c2 => {
      apply(e)(c1)(c2) match {
        case TyRef(t) => t
        case TySource(t) => t
        case _ => typeError()
      }
    }

  override def tmAssign(l: Exp2[A, Exp[B]], r: Exp2[A, Exp[B]]): (Ctx[Int, Exp[B]]) => Type[B] =
    c1 => c2 => {
      val tl = apply(l)(c1)(c2)
      val tr = apply(r)(c1)(c2)
      if (tl(subtypeOf)(TySink(tr))) TyUnit[B]() else typeError()
    }
}

object Typer extends Typer[Alg, TAlg] with Impl[Ctx[Int, Exp[TAlg]] => Type[TAlg]] {
  override val tEquals: (Exp[TAlg]) => (Exp[TAlg]) => Boolean = _ (TEquals)

  override val join: TAlg[Exp[TAlg], (Exp[TAlg]) => Exp[TAlg]] = Join

  override val subtypeOf: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] = SubtypeOf
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with fullsub.TEquals[A] with variant.TEquals[A] with ref.TEquals[A] {

  override def tySource(t: Exp[A]): Exp[A] => Boolean = {
    case TySource(t2) if apply(t)(t2) => true
    case _ => false
  }

  override def tySink(t: Exp[A]): Exp[A] => Boolean = {
    case TySink(t2) if apply(t)(t2) => true
    case _ => false
  }
}

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with fullsub.SubtypeOf[A] with variant.SubtypeOf[A] {

  override def tySource(t: Exp[A]): Exp[A] => Boolean = {
    case TySource(t2) => apply(t)(t2)
    case TyTop() => true
    case _ => false
  }

  override def tySink(t: Exp[A]): Exp[A] => Boolean = {
    case TySink(t2) => apply(t2)(t)
    case TyTop() => true
    case _ => false
  }

  override def tyRef(t: Exp[A]): Exp[A] => Boolean = {
    case TyRef(t2) => apply(t)(t2) && apply(t2)(t)
    case TySource(t2) => apply(t)(t2)
    case TySink(t2) => apply(t2)(t)
    case TyTop() => true
    case _ => false
  }
}

object SubtypeOf extends SubtypeOf[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with fullsub.Join[A] with variant.Join[A] {

  override def tySource(t: Exp[A]): (Exp[A]) => Exp[A] = {
    case TySource(t2) => TySource(apply(t)(t2))
    case TyRef(t2) => TySource(apply(t)(t2))
    case _ => TyTop()
  }

  override def tySink(t: Exp[A]): (Exp[A]) => Exp[A] = {
    case TySink(t2) => TySink(t(meet)(t2))
    case TyRef(t2) => TySink(t(meet)(t2))
    case _ => TyTop()
  }

  override def tyRef(t: Exp[A]): (Exp[A]) => Exp[A] = {
    case TySource(t2) => TySource(apply(t)(t2))
    case TySink(t2) => TySink(t(meet)(t2))
    case TyRef(t2) => if (t(subtypeOf)(t2) && t2(subtypeOf)(t)) TyRef(t) else TySource(apply(t)(t2))
    case _ => TyTop()
  }
}

object Join extends Join[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val meet: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Meet
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with fullsub.Meet[A] {
  override def tyVariant(l: List[(String, Exp[A])]): Exp[A] => Exp[A] = u =>
    directMeet(TyVariant[A](l), u).getOrElse(u match {
      case TyVariant(l2) =>
        val o1 = l.filter(b => l2.forall(_._1 != b._1))
        val o2 = l2.filter(b => l.forall(_._1 != b._1))
        val i = for {
          (n, t) <- l
          p = l2.find(_._1 == n)
          if p.nonEmpty
        } yield (n, apply(t)(p.get._2))
        TyVariant[A](o1 ++ o2 ++ i)
      case _ => default
    })

  override def tySource(t: Exp[A]): (Exp[A]) => Exp[A] = {
    case TySource(t2) => TySource(apply(t)(t2))
    case TyRef(t2) => TySource(apply(t)(t2))
    case _ => default
  }

  override def tySink(t: Exp[A]): (Exp[A]) => Exp[A] = {
    case TySink(t2) => TySink(t(join)(t2))
    case TySource(t2) => TySink(t(join)(t2))
    case _ => default
  }

  override def tyRef(t: Exp[A]): (Exp[A]) => Exp[A] = {
    case TySource(t2) => TySource(apply(t)(t2))
    case TySink(t2) => TySink(t(join)(t2))
    case TyRef(t2) => if (t(subtypeOf)(t2) && t2(subtypeOf)(t)) TyRef(t) else TySource(apply(t)(t2))
    case _ => default
  }
}

object Meet extends Meet[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val join: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Join
}
