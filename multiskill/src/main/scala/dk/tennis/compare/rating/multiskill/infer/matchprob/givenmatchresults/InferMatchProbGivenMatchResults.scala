package dk.tennis.compare.rating.multiskill.infer.matchprob.givenmatchresults

import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.model.matchmodel.MatchPrediction
import dk.tennis.compare.rating.multiskill.model.perfdiff.Score
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.skillcov.SkillCovFunc
import dk.tennis.compare.rating.multiskill.infer.skillmodelparams.SkillsModelParams
import dk.tennis.compare.rating.multiskill.infer.skillgivenplayer.InferSkillGivenPlayer
import dk.tennis.compare.rating.multiskill.infer.perfdiffgivenskills.inferPerfDiffGivenSkills
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player

case class InferMatchProbGivenMatchResults(matchResults: IndexedSeq[MatchResult]) {

  private val (priorSkillOnServe, priorSkillOnReturn) = (4.65d, 0)
  val skillCovParams = Array(
    2.022132402833459, // opponent 
    2.335108140472381, 0.8585245019924235, 0.6068550430203135, // surface
    -0.7964151980207621, 3.0080055487894057, 0.422376471909165, 7.932430851981252 //time
    )
  private val logPerfStdDev = 2.3

  val allScores = Score.toScores(matchResults)
  val skillCovFunc = SkillCovFunc(skillCovParams)
  val skillsModelParams = SkillsModelParams(playerSkillMeanPrior, skillCovFunc)
  val infer = InferSkillGivenPlayer(skillsModelParams, logPerfStdDev, allScores)

  def predict(matchResult: MatchResult): MatchPrediction = {
    val predictionscores = Score.toScores(List(matchResult))

    val p1SkillOnServe = infer.inferSkill(predictionscores(0).player1)
    val p2SkillOnReturn = infer.inferSkill(predictionscores(0).player2)

    val p2SkillOnServe = infer.inferSkill(predictionscores(1).player1)
    val p1SkillOnReturn = infer.inferSkill(predictionscores(1).player2)

    val p1OnServeGamePerfDiff = inferPerfDiffGivenSkills(p1SkillOnServe.skill, p2SkillOnReturn.skill, logPerfStdDev)
    val p2OnServeGamePerfDiff = inferPerfDiffGivenSkills(p2SkillOnServe.skill, p1SkillOnReturn.skill, logPerfStdDev)

    val matchPrediction = MatchPrediction(predictionscores(0), predictionscores(1), p1OnServeGamePerfDiff, p2OnServeGamePerfDiff, matchResult)
    matchPrediction
  }

  private def playerSkillMeanPrior(player: Player): Double = {
    if (player.onServe) priorSkillOnServe else priorSkillOnReturn

  }
}