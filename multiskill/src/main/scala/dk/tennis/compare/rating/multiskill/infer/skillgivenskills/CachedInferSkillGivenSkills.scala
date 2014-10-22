package dk.tennis.compare.rating.multiskill.infer.skillgivenskills

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.perfdiff.Player
import dk.bayes.math.linear.Matrix
import dk.bayes.math.linear.Matrix
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.CovFunc
import dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.PlayerSkills
import scala.collection.mutable.HashMap
import scala.collection._

/**
 * Infer player skill given other players skills
 */
case class CachedInferSkillGivenSkills(getPlayerSkillsForPlayer: (String) => PlayerSkills, playerCovFunc: CovFunc, skillMeanFunc: (Player) => Double) {

  private val inferByPlayerMap: mutable.Map[String, InferSkillGivenSkills] = new HashMap()

  def infer(z: Player): Gaussian = {

    def getInfer(playerName: String): InferSkillGivenSkills = {
//println(playerName)
      val playerSkills = getPlayerSkillsForPlayer(z.playerName)
      val infer = InferSkillGivenSkills(playerSkills, playerCovFunc, skillMeanFunc)
      infer
    }

    val infer = inferByPlayerMap.getOrElseUpdate(z.playerName, getInfer(z.playerName))

    infer.infer(z)
  }

}