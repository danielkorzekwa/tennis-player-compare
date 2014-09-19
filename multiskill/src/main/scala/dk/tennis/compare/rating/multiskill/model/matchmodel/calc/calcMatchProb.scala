package dk.tennis.compare.rating.multiskill.model.matchmodel.calc

import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.math.gameSkillsToPerfDiffs
import dk.tennis.compare.rating.multiskill.model.perfdiff.math.gameSkillToPerfDiff
import scala.math._
import dk.tennis.compare.rating.multiskill.model.outcomelik.OutcomeLik
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._

object calcMatchProb {

  def apply(p1SkillOnServe: Gaussian, p1SkillOnReturn: Gaussian, p2SkillOnServe: Gaussian, p2SkillOnReturn: Gaussian,
    logPerfStdDev: Double, numOfSets: Int): Double = {

    val p1OnServeGamePerfDiff = gameSkillToPerfDiff(p1SkillOnServe, p2SkillOnReturn, logPerfStdDev)
    val p2OnServeGamePerfDiff = gameSkillToPerfDiff(p2SkillOnServe, p1SkillOnReturn, logPerfStdDev)

    val p1ProbOnServe = exp(OutcomeLik.loglik(p1OnServeGamePerfDiff.perfDiff, true))
    val p2ProbOnServe = exp(OutcomeLik.loglik(p2OnServeGamePerfDiff.perfDiff, true))

    val matchType = if (numOfSets == 2) THREE_SET_MATCH
    else if (numOfSets == 3) FIVE_SET_MATCH
    else throw new IllegalArgumentException("Incorrect number of sets")

    val matchProb = calcMatchProb(p1OnServeGamePerfDiff.perfDiff, p2OnServeGamePerfDiff.perfDiff, numOfSets)
    matchProb
  }

  def apply(p1OnServeGamePerfDiff: Gaussian, p2OnServeGamePerfDiff: Gaussian, numOfSets: Int): Double = {

    val p1ProbOnServe = exp(OutcomeLik.loglik(p1OnServeGamePerfDiff, true))
    val p2ProbOnServe = exp(OutcomeLik.loglik(p2OnServeGamePerfDiff, true))

    val matchType = if (numOfSets == 2) THREE_SET_MATCH
    else if (numOfSets == 3) FIVE_SET_MATCH
    else throw new IllegalArgumentException("Incorrect number of sets")

    val matchProb = TennisProbFormulaCalc.matchProb(p1ProbOnServe, 1 - p2ProbOnServe, matchType)

    matchProb
  }
}