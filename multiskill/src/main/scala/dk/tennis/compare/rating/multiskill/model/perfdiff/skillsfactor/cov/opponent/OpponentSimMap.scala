package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import dk.bayes.infer.gp.cov.CovSEiso
import dk.bayes.math.linear.Matrix

case class OpponentSimMap(id: String, getPlayerSkills: String => Array[Double], opponentCovFunc: CovSEiso) {

  //key - opponentName1, map[opponentName2, cov value]]
  private val covMap: Map[String, Map[String, OpponentCovValue]] = new HashMap()

  def getCovValue(opponentName1: String, opponentName2: String): OpponentCovValue = {

    def calcCovValue(): OpponentCovValue = {
      val skillsGivenOpponent1 = getPlayerSkills(opponentName1)
      val skillsGivenOpponent2 = getPlayerSkills(opponentName2)

      val covVal = opponentCovFunc.cov(skillsGivenOpponent1, skillsGivenOpponent2)
      val df_dSf = 0 //opponentCovFunc.df_dSf(Matrix(skillsGivenOpponent1), Matrix(skillsGivenOpponent2))
      val df_dEll = 0d //opponentCovFunc.df_dEll(Matrix(skillsGivenOpponent1), Matrix(skillsGivenOpponent2))

//      println(opponentName1, opponentName2)
//      println(skillsGivenOpponent1.toList)
//      println(skillsGivenOpponent2.toList)
//      println(covVal)
//      println("-----------------------")

      OpponentCovValue(covVal, df_dSf, df_dEll)
    }

    val covValue = covMap.getOrElseUpdate(opponentName1, new HashMap()).getOrElseUpdate(opponentName2, calcCovValue())
    covValue
  }
}