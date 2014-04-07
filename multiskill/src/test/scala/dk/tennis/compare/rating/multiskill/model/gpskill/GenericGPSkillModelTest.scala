package dk.tennis.compare.rating.multiskill.model.gpskill

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.gpskill.factorgraph.GenericGPSkillsInfer
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.bayes.math.gaussian.Gaussian

class GenericGPSkillModelTest extends Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2009, 2011).take(5)

  val initialSkillsOnServe = PlayerSkill(0, 0.8806472793221474)
  val initialSkillsOnReturn = PlayerSkill(3997.9909513252546 - 4002.542974700307, 0.7527376376092434)
  val skillOnServeTransVariance = 0.0005
  val skillOnReturnTransVariance = 0
  val pointPerfVarianceOnServe = 102.61914136268837
  val pointPerfVarianceOnReturn = 102.61914136268837

  val meanFunc = new MeanFunc {
    def mean(skillOnServe: Boolean): Double = {
      if (skillOnServe) initialSkillsOnServe.mean
      else initialSkillsOnReturn.mean

    }
  }

  val varFunc = new CovFunc {
    def cov(playerSkill1: PlayerGPSkill, playerSkill2: PlayerGPSkill): Double = {

      val diag = if (playerSkill1.equals(playerSkill2)) 0.0001 else 0

      val theSamePlayerSkill = if (playerSkill1.player.equals(playerSkill2.player) && playerSkill1.skillOnServe == playerSkill2.skillOnServe) {

        if (playerSkill1.skillOnServe) initialSkillsOnServe.variance
        else initialSkillsOnReturn.variance

      } else 0

      val theSameSkillType = if (playerSkill1.skillOnServe == playerSkill2.skillOnServe) 0.1 else 0
      diag + theSamePlayerSkill + theSameSkillType
    }
  }

  @Test def test {
    logger.info("Building prior gp skills")
    val priorGPSkills = GPSkills(tournaments, meanFunc, varFunc)
    logger.info("All players: " + priorGPSkills.allPlayers.size)
    logger.info("All matches: " + priorGPSkills.matchResults.size)

    val infer = GenericGPSkillsInfer(pointPerfVarianceOnServe, pointPerfVarianceOnReturn)
    val skillsGaussianMarginal = infer.skillsMarginal(priorGPSkills, threshold = 0.6)

    val skills = priorGPSkills.copy(skillsGaussian = skillsGaussianMarginal)

    val matchSkills = tournaments.flatMap { t =>

      val matchSkills = t.matchResults.map { r =>
        val p1Skills = skills.playerSkills(r.player1, r, t)
        val p2Skills = skills.playerSkills(r.player2, r, t)
        MatchSkills(r, p1Skills, p2Skills)
      }
      matchSkills
    }

    val predictions: Seq[Tuple3[Double, Int, Int]] = calcPredictions(matchSkills)

    println("Log lik(totalLik,avgLik,pointsTotal): " + LogLik.logLik(predictions))
  }

  private def calcPredictions(matchSkills: Seq[MatchSkills]): Seq[Tuple3[Double, Int, Int]] = {
    val (pointPerfVarianceOnServe, pointPerfVarianceOnReturn) = (195.61914136268837, 155)

    val predictions: Seq[Tuple3[Double, Int, Int]] = matchSkills.flatMap { m =>

      val genericPointModel = GenericPointModel(pointPerfVarianceOnServe, pointPerfVarianceOnReturn)

      val p1PointProb = genericPointModel.pointProb(m.player1Skills.skillOnServe, m.player2Skills.skillOnReturn)
      val p2PointProb = genericPointModel.pointProb(m.player2Skills.skillOnServe, m.player1Skills.skillOnReturn)

      List(

        Tuple3(p1PointProb, m.result.p1Stats.servicePointsWon, m.result.p1Stats.servicePointsTotal),
        Tuple3(p2PointProb, m.result.p2Stats.servicePointsWon, m.result.p2Stats.servicePointsTotal))
    }
    predictions
  }
  
  private implicit def toGaussian(playerSkill:PlayerSkill):Gaussian = Gaussian(playerSkill.mean,playerSkill.variance)
}