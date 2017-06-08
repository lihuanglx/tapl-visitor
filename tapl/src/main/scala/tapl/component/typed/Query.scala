package tapl.component.typed

import tapl.component.varapp

trait Query[R, E, F] extends Alg[R, E, F] with varapp.Alg.Query[R, E] {
  override def TmAbs(x: String, t: F, e: R): E = default
}
