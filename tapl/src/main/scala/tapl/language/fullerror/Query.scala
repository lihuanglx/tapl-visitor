package tapl.language.fullerror

import tapl.component.typedbool
import tapl.language.bot

trait Query[R, E, F] extends Alg[R, E, F] with bot.Query[R, E, F] with typedbool.Alg.Query[R, E] {
  override def TmError(): E = default

  override def TmTry(e1: R, e2: R): E = default
}
