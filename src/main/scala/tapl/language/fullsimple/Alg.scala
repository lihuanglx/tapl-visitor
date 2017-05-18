package tapl.language.fullsimple

import tapl.component.{simple, variant}

trait Alg[-R, E, -F] extends simple.Alg[R, E, F] with variant.Alg[R, E, F]

trait Factory extends simple.Factory with variant.Factory

object Factory extends Factory

trait TFactory extends simple.TFactory with variant.TFactory

object TFactory extends TFactory
