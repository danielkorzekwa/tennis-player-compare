package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.gaussian.Gaussian
import scala.math._

case class OpponentSimMap(id: String, getPlayerSkills: String => Array[Gaussian], opponentCovFunc: CovSEiso) {

  //key - opponentName1, map[opponentName2, cov value]]
  private val covMap: Map[String, Map[String, OpponentCovValue]] = new HashMap()

  def getCovValue(opponentName1: String, opponentName2: String): OpponentCovValue = {

    def calcCovValue(): OpponentCovValue = {
      val skillsGivenOpponent1 = getPlayerSkills(opponentName1).map(v => v.m)
      val skillsGivenOpponent2 = getPlayerSkills(opponentName2).map(v => v.m)

//      val skills1 = getPlayerSkills(opponentName1)
//      val skills2 = getPlayerSkills(opponentName2)
//
//      val skillsGivenOpponent1 = skills1.zip(skills2).map {
//        case (s1, s2) =>
//          val skillDiff = (s1 - s2)
//          val perfDiff = skillDiff + Gaussian(0, 2 * pow(exp(2.3), 2))
//          val v = exp(OutcomeLik.loglik(perfDiff, true)) - 0.5
//          v
//      }
//      val skillsGivenOpponent2 = skills2.map(v => 0d)

      val covVal = opponentCovFunc.cov(skillsGivenOpponent1, skillsGivenOpponent2)
      val df_dSf = opponentCovFunc.df_dSf(skillsGivenOpponent1, skillsGivenOpponent2)
      val df_dEll = opponentCovFunc.df_dEll(skillsGivenOpponent1, skillsGivenOpponent2)

      if (!id.equals("onServe")) OpponentCovValue(covVal, df_dSf, df_dEll)
      else OpponentCovValue(0, 0, 0)
      //    OpponentCovValue(covVal, df_dSf, df_dEll)
    }

    val covValue = covMap.getOrElseUpdate(opponentName1, new HashMap()).getOrElseUpdate(opponentName2, calcCovValue())
    covValue
  }
}