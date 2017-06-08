package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with fullsimple.Typer[A, B] {

  override def TmFold(e: E3[A, Exp[B]], t: Exp[B]): Type[B] = ???

  override def TmUnfold(e: E3[A, Exp[B]], t: Exp[B]): Type[B] = ???
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = ???
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with fullsimple.TEquals[A] {

}
