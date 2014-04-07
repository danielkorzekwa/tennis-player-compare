package dk.tennis.compare.rating.multiskill.model.gpskill

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

 case class PlayerGPSkill(player: String, skillOnServe: Boolean, matchResult: MatchResult)