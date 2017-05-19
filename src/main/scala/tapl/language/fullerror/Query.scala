package tapl.language.fullerror

import tapl.component.{typed2, typedbool}

trait Query[R, E, F] extends Alg[R, E, F] with typed2.Query[R, E, F] with typedbool.Query[R, E] {
  override def TmError(): E = default

  override def TmTry(e1: R, e2: R): E = default
}
