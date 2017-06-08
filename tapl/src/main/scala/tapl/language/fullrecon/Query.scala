package tapl.language.fullrecon

import tapl.component.let
import tapl.language.recon

trait Query[R, E, F] extends Alg[R, E, F] with recon.Query[R, E, F] with let.Query[R, E]
