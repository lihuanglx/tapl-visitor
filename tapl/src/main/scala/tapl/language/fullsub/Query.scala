package tapl.language.fullsub

import tapl.component.simple

trait Query[R, E, F] extends Alg[R, E, F] with simple.Alg.Query[R, E, F]
