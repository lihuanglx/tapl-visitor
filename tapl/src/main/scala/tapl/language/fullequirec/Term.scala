package tapl.language.fullequirec

import gems.Language
import tapl.common._
import tapl.component.{extension, variant}
import tapl.language.equirec

@Language
trait Term[-R, E, -F] extends equirec.Term[R, E, F] with extension.Term[R, E, F] with variant.Term[R, E, F]

@Language
trait Type[-F, T] extends equirec.Type[F, T] with extension.Type[F, T] with variant.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
