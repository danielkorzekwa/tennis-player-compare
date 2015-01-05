package dk.tennis.compare.analysis

import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.infer.skills.InferSkill
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.infer.skills.givenallmatches.InferSkillGivenAllMatches
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent.PlayerSkill
import java.text.SimpleDateFormat
import dk.tennis.compare.rating.multiskill.model.perfdiff.Surface
import dk.tennis.compare.rating.multiskill.model.perfdiff.NumOfSets
import com.typesafe.scalalogging.slf4j.Logging

object plotAvgSkillsOverTimeApp extends App with Logging {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val matchResults = MatchesLoader.loadMatches(matchesFile, 2008, 2011) //.take(300)
  val scores = Score.toScores(matchResults)

  logger.info("Building model...")
  val model = getSkillModel1()

  //get players for analysis
  val df = new SimpleDateFormat("dd/MM/yyyy")
  val time = df.parse("12/12/2011")
  val players = scores.map(s => s.player1.playerName).distinct
  val playerSkills = playersOnServeRanking(players, model, time)
  val filteredPlayers = playerSkills.take(50).map(s => s.player.playerName).toSet

  plotAvgSkillsOverTime(scores, filteredPlayers, model)

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

}

