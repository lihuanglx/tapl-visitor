package tapl.language.fullisorec

import macros.Language
import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

@Language
trait Term[-R, E, -F] extends fullsimple.Term[R, E, F] {
  def tmFold(e: R, t: F): E

  def tmUnfold(e: R, t: F): E
}

@Language
trait Type[-F, T] extends fullsimple.Type[F, T] with rectype.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
