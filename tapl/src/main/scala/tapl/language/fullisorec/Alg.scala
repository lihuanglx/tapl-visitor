package tapl.language.fullisorec

import macros.Visitor
import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

@Visitor
trait Alg[-R, E, -F] extends fullsimple.Alg[R, E, F] {
  def tmFold(e: R, t: F): E

  def tmUnfold(e: R, t: F): E
}

@Visitor
trait TAlg[-F, T] extends fullsimple.TAlg[F, T] with rectype.TAlg[F, T]

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
