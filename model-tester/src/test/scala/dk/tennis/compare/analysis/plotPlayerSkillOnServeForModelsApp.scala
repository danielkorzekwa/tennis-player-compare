package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.infer.skills.givenallmatches.InferSkillGivenAllMatches
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import com.typesafe.scalalogging.slf4j.Logging
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillSumCovFunc
import java.text.SimpleDateFormat

/**
 *  Plot skills on serve for a given player.
 */
object plotPlayerSkillOnServeForModelsApp extends App with Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2008, 2011)//.take(300)
  val scores = Score.toScores(matchResults)

  logger.info("Building models...")
  val models = Map(
    "mult" -> getSkillModel1)

  //get players for analysis
  val df = new SimpleDateFormat("dd/MM/yyyy")
  val time = df.parse("12/12/2011")
  val players = scores.map(s => s.player1.playerName).distinct
  val playerSkills = playersOnServeRanking(players, models("mult"), time)
  val filteredPlayers = playerSkills.takeRight(10).map(s => s.player.playerName)

  // val players = List("Roger Federer","Richard Gasquet")
  logger.info("Plotting models...")
  plotPlayerSkillOnServeForModels(scores, filteredPlayers, models, legend = true)

  private def getSkillModel1(): InferSkill = {

    val (priorSkillOnServe, priorSkillOnReturn) = (4.64, 0.00)
    def playerSkillMeanPrior(player: Player): Double = {
      if (player.onServe) priorSkillOnServe else priorSkillOnReturn
    }

    val skillCovParams = Array(2.359812754065542, 2.2021945860904166, 0.7336945892183001, 1.9400983181062423, -0.30262409579071925, 2.6601797657125483, 0.4245995628062712, 7.152345506936956)
    val skillCovFunc = SkillCovFunc(skillCovParams)

    val logPerfStdDev = 2.3

    val skillsModelParams = SkillsModelParams(playerSkillMeanPrior, skillCovFunc)

    InferSkillGivenAllMatches(skillsModelParams, logPerfStdDev, scores)
  }

  private def getSkillModel2(): InferSkill = {
    val (priorSkillOnServe, priorSkillOnReturn) = (4.73d, 0)
    def playerSkillMeanPrior(player: Player): Double = {
      if (player.onServe) priorSkillOnServe else priorSkillOnReturn
    }

    //   val skillCovParams = Array(-0.06752175746252553, -1.0113707529339377, 2.4488187495488805, 0.026290720424598603, 2.294220360249113, 2.232563518848958, 2.2819016681372464, -0.7620553457309178, 2.6343120756382894, -0.2905464442409732, 6.228026770395624)
    val skillCovParams = Array(-0.06752175746252553, -100.0113707529339377, 2.4488187495488805, -100.026290720424598603, 2.294220360249113, 2.232563518848958, 2.2819016681372464, -0.7620553457309178, 2.6343120756382894, -0.2905464442409732, 6.228026770395624)

    val skillCovFunc = SkillSumCovFunc(skillCovParams)

    val logPerfStdDev = 2.3

    val skillsModelParams = SkillsModelParams(playerSkillMeanPrior, skillCovFunc)

    InferSkillGivenAllMatches(skillsModelParams, logPerfStdDev, scores)
  }
}