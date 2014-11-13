package dk.tennis.compare

import dk.bayes.math.gaussian.Gaussian
import dk.tennis.compare.rating.multiskill.infer.matchprob.givenskills.inferMatchProbGivenSkills

object MatchProbGivenSkills extends App {

  val p1OnServe = Gaussian(8.0, 0)
  val p1OnReturn = Gaussian(2.30, 0)

  val p2OnServe = Gaussian(7.4, 0)
  val p2OnReturn = Gaussian(4.25, 0)

  val p1MatchProb = inferMatchProbGivenSkills(p1OnServe, p1OnReturn, p2OnServe, p2OnReturn, logPerfStdDev = 2.3, 2)

  println("matchProb =" + p1MatchProb)
}