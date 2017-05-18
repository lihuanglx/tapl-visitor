package tapl.language.fullsimple

import tapl.component.{simple, variant}

trait Query[R, E, F] extends Alg[R, E, F] with simple.Query[R, E, F] with variant.Query[R, E, F]
