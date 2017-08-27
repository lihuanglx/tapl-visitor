package inspect.language.untyped

import inspect.common._
import inspect.component.varapp
import macros.Lang

@Lang("untyped")
trait Term[-R, E] extends varapp.Term[R, E] {
  def tmAbs(x: String, e: R): E
}

trait Impl[T] extends Term[Exp[Term], T] with Term.Inspect[Term] {
  override def apply(e: Exp[Term]): T = e(this)

  override def inspectUntyped[B[-R, _]](e: SExp[Term, B]): Option[SExp[Term, B]] = Some(e)
}
