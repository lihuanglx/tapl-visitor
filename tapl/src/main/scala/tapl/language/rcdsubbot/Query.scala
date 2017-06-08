package tapl.language.rcdsubbot

import tapl.component.typedrecord
import tapl.language.bot

trait Query[R, E, F] extends Alg[R, E, F] with bot.Query[R, E, F] with typedrecord.Query[R, E]
