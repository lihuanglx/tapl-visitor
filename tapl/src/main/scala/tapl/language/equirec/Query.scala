package tapl.language.equirec

import tapl.component.typed

trait Query[R, E, F] extends Alg[R, E, F] with typed.Alg.Query[R, E, F]
