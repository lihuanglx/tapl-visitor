package tapl.language.untyped

import tapl.component.varapp

trait Query[R, T] extends Alg[R, T] with varapp.Query[R, T] {
  override def TmAbs(x: String, e: R): T = default
}
