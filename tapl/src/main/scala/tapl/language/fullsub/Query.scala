package tapl.language.fullsub

import tapl.component.simple

trait Query[R, E, F] extends Alg[R, E, F] with simple.Query[R, E, F]
