package dk.tennis.compare.rating.multiskill.model.gpskill

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult

trait GPSkill {

  def calcMatchSkills(tournaments: Seq[TournamentResult]):Seq[MatchSkills]
}