package tapl.language.bot

import tapl.component.typed

trait Query[R, E, F] extends Alg[R, E, F] with typed.Query[R, E, F]
