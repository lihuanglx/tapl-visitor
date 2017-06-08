package tapl.language.fullisorec

import tapl.language.fullsimple

trait Query[R, E, F] extends Alg[R, E, F] with fullsimple.Query[R, E, F] {
  override def TmFold(e: R, t: F): E = default

  override def TmUnfold(e: R, t: F): E = default
}
