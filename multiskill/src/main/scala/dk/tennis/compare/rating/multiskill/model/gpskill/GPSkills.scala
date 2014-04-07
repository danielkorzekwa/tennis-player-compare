package dk.tennis.compare.rating.multiskill.model.gpskill

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import com.typesafe.scalalogging.slf4j.Logging
import dk.bayes.math.gaussian.MultivariateGaussian

case class GPSkills(matchResults: Seq[MatchResult], skillsGaussian: MultivariateGaussian) extends Logging {

  val allPlayers = matchResults.flatMap(m => List(m.player1, m.player2)).distinct.toIndexedSeq

  require(skillsGaussian.v.numRows == matchResults.size * 4, "The dimensionality of skills gaussian must equal to the matchesNum*4")

  def playerSkills(player: String, matchResult: MatchResult, tournament: TournamentResult): PlayerSkills = {
    val indexOnServe = skillIndexOnServe(player, matchResult)
    val skillOnServe = PlayerSkill(skillsGaussian.m(indexOnServe), skillsGaussian.v(indexOnServe, indexOnServe))

    val indexOnReturn = skillIndexOnReturn(player, matchResult)
    val skillOnReturn = PlayerSkill(skillsGaussian.m(indexOnReturn), skillsGaussian.v(indexOnReturn, indexOnReturn))

    PlayerSkills(player, matchResult.matchTime, skillOnServe, skillOnReturn)
  }

  def skillIndexOnServe(player: String, matchResult: MatchResult): Int = {
    GPSkills.skillIndexOnServe(player, matchResult, matchResults)
  }
  def skillIndexOnReturn(player: String, matchResult: MatchResult): Int = {
    GPSkills.skillIndexOnReturn(player, matchResult, matchResults)
  }

}

object GPSkills {
  def apply(tournaments: Seq[TournamentResult], meanFunc: MeanFunc, covFunc: CovFunc): GPSkills = {
    val allMatches = tournaments.flatMap(t => t.matchResults)

    val mean: Array[Double] = tournaments.flatMap { t =>
      t.matchResults.flatMap { r =>
        List(meanFunc.mean(true), meanFunc.mean(false),
          meanFunc.mean(true), meanFunc.mean(false))
      }
    }.toArray

    def covariance(rowIndex: Int, colIndex: Int): Double = {
      val playerA = playerSkill(rowIndex, allMatches)
      val playerB = playerSkill(colIndex, allMatches)
      val cov = covFunc.cov(playerA, playerB)

      cov
    }

    val skillsVar = Matrix(mean.size, mean.size, covariance(_, _))
    val skillsGaussian = MultivariateGaussian(Matrix(mean), skillsVar)
    new GPSkills(allMatches, skillsGaussian)
  }

  def playerSkill(skillIndex: Int, allMatches: Seq[MatchResult]): PlayerGPSkill = {

    val matchResult = allMatches(skillIndex / 4)
    val playerSkill = (skillIndex % 4) match {
      case 0 => PlayerGPSkill(matchResult.player1, true, matchResult)
      case 1 => PlayerGPSkill(matchResult.player1, false, matchResult)
      case 2 => PlayerGPSkill(matchResult.player2, true, matchResult)
      case 3 => PlayerGPSkill(matchResult.player2, false, matchResult)
      case _ => throw new IllegalStateException("Incorrect skill index")
    }
    playerSkill

  }

  def skillIndexOnServe(player: String, matchResult: MatchResult, allMatches: Seq[MatchResult]): Int = {
    val matchIndex = allMatches.indexOf(matchResult)

    val index = if (player.equals(matchResult.player1)) 4 * matchIndex + 0
    else if (player.equals(matchResult.player2)) 4 * matchIndex + 2
    else throw new IllegalArgumentException("Unknown player")

    index
  }
  def skillIndexOnReturn(player: String, matchResult: MatchResult, allMatches: Seq[MatchResult]): Int = {
    val matchIndex = allMatches.indexOf(matchResult)

    val index = if (player.equals(matchResult.player1)) 4 * matchIndex + 1
    else if (player.equals(matchResult.player2)) 4 * matchIndex + 3
    else throw new IllegalArgumentException("Unknown player")

    index
  }

}