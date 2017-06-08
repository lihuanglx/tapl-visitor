package tapl.language.fullequirec

import tapl.language.fullsimple

trait Query[R, E, F] extends Alg[R, E, F] with fullsimple.Alg.Query[R, E, F]
