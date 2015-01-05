package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.infer.skills.givenallmatches.InferSkillGivenAllMatches
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.scoresim.ScoresSimulator
import dk.tennis.compare.rating.multiskill.infer.skills.giventrueskills.InferSkillGivenTrueSkills
import dk.tennis.compare.rating.multiskill.scoresim.SimScore
import dk.bayes.math.gaussian.Gaussian
import scala.math._
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillSumCovFunc

/**
 *  Plot skills on serve for a given player (true skill versus predicted skill).
 */
object plotSimPlayerSkillOnServeForModelsApp extends App with Logging {

  val simScores = getSimScores()
  val scores = simScores.map(s => s.score).toArray
  logger.info("All scores:" + scores.size)

  logger.info("Building models...")
  val models = Map(
    "trueModel" -> getTrueModel(),
    "currentModel" -> getCurrentModel())

  logger.info("Plotting models...")
  plotPlayerSkillOnServeForModels(simScores.map(s => s.score), List("Roger Federer"), models,legend=true)

  private def getCurrentModel(): InferSkill = {

    val (skillsModelParams, logPerfStdDev) = getCurrSkillsModelParams()
    InferSkillGivenAllMatches(skillsModelParams, logPerfStdDev, scores)
  }

  private def getTrueModel(): InferSkill = {
    val trueSkillMap: Map[Player, Gaussian] = simScores.map(s => s.score.player1 -> s.gameSkills.marginal(0)).toMap
    InferSkillGivenTrueSkills(trueSkillMap)
  }

  private def getSimScores(): Seq[SimScore] = {
    val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
    val realMatchResults = MatchesLoader.loadMatches(matchesFile, 2011, 2011)//.take(800)
    val realScores: Array[Score] = Score.toScores(realMatchResults)

    logger.info("Simulating scores...")
    val scoresSimulator = ScoresSimulator()
    val (skillsModelParams, logPerfStdDev) = getSimSkillsModelParams()
    val (simScores, trueLoglik) = scoresSimulator.simulate(realScores, Map(), randSeed = 5644567, skillsModelParams, logPerfStdDev)
    simScores
  }

  private def getSimSkillsModelParams(): Tuple2[SkillsModelParams, Double] = {

    val (priorSkillOnServe, priorSkillOnReturn) = (5, 0)
    def playerSkillMeanPrior(player: Player): Double = {
      if (player.onServe) priorSkillOnServe else priorSkillOnReturn
    }

    val skillCovParams = Array(
        log(1),
        log(0.2),log(2),log(2), 
        log(2), 
        log(0.2), log(2),log(2),log(2)
      )
      
//       val skillCovParams = Array(
//      2.022132402833459, // opponent 
//      2.335108140472381, 0.8585245019924235, 0.6068550430203135, // surface
//      -0.7964151980207621, 3.0080055487894057, 0.422376471909165, 7.932430851981252 //time
//      )
    val skillCovFunc = SkillSumCovFunc(skillCovParams)

    val logPerfStdDev = log(10)

    val skillsModelParams = SkillsModelParams(playerSkillMeanPrior, skillCovFunc)

    (skillsModelParams, logPerfStdDev)
  }
  
   private def getCurrSkillsModelParams(): Tuple2[SkillsModelParams, Double] = {

    val (priorSkillOnServe, priorSkillOnReturn) = (5, 0)
    def playerSkillMeanPrior(player: Player): Double = {
      if (player.onServe) priorSkillOnServe else priorSkillOnReturn
    }

    val skillCovParams = Array(
        log(2),
        log(0.2),log(2),log(2), 
        log(2), 
        log(0.2), log(2),log(2),log(2)
      )
      
//       val skillCovParams = Array(
//      2.022132402833459, // opponent 
//      2.335108140472381, 0.8585245019924235, 0.6068550430203135, // surface
//      -0.7964151980207621, 3.0080055487894057, 0.422376471909165, 7.932430851981252 //time
//      )
    val skillCovFunc = SkillSumCovFunc(skillCovParams)

    val logPerfStdDev = log(10)

    val skillsModelParams = SkillsModelParams(playerSkillMeanPrior, skillCovFunc)

    (skillsModelParams, logPerfStdDev)
  }
}