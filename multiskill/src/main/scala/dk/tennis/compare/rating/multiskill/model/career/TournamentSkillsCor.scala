package dk.tennis.compare.rating.multiskill.model.career

import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.bayes.math.gaussian.Linear._
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

/**
 * @param tournament
 * @param allPlayers
 * @param skillsGaussian Skills on serve and return
 */
case class TournamentSkillsCor(tournament: TournamentResult, allPlayers: Seq[String],
  skillsGaussian: CanonicalGaussian) {

  def initialPlayerSkills(): Map[String, PlayerSkills] = {

    val (skillsMean, skillsVariance) = skillsGaussian.getMeanAndVariance()
    
    val skillsMap = allPlayers.zipWithIndex.map {
      case (p, index) =>

        val skillOnServeMean = skillsMean(index)
        val skillOnServeVar = skillsVariance(index, index)

        val skillOnReturnMean = skillsMean(allPlayers.size + index)
        val skillOnReturnVar = skillsVariance(allPlayers.size + index, allPlayers.size + index)

        val playerSkills = PlayerSkills(p, tournament.tournamentTime, PlayerSkill(skillOnServeMean, skillOnServeVar), PlayerSkill(skillOnReturnMean, skillOnReturnVar))
        (p, playerSkills)
    }.toMap

    skillsMap
  }
}