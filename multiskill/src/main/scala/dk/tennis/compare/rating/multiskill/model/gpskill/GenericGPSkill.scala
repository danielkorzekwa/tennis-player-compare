package dk.tennis.compare.rating.multiskill.model.gpskill

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult

class GenericGPSkill extends GPSkill {

   def calcMatchSkills(tournaments: Seq[TournamentResult]):Seq[MatchSkills] = {
     Nil
   }
}