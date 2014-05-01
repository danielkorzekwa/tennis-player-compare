package dk.tennis.compare.rating.multiskill.model.fastgpskill

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.Matrix
import dk.bayes.math.linear._
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.fastgpskill.factorgraph.GenericSkillsFactorGraph
import scala.math._
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.Random
import dk.tennis.compare.rating.multiskill.matchloader.Player
import dk.tennis.compare.rating.multiskill.model.priorskills.PriorSkills

case class GenericFastGpSkill(initialSkillsOnServe: Gaussian, initialSkillsOnReturn: Gaussian,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends FastGpSkill with Logging {

  def skillMarginals(players: Array[Player]): Seq[Gaussian] = {

    val supportPlayers = new Random(4565654645L).shuffle(players.toSeq).take(200).toArray

    val priorSupportSkills = PriorSkills.priorSkills(supportPlayers, initialSkillsOnServe, initialSkillsOnReturn)

    val allVsSupportCov = PriorSkills.covarianceMatrix(players, supportPlayers, initialSkillsOnServe.v, initialSkillsOnReturn.v)

    val factorGraph = GenericSkillsFactorGraph(priorSupportSkills, allVsSupportCov, players, perfVarianceOnServe, perfVarianceOnReturn)
    for (i <- 1 to 3) {
      logger.info("Sending messages...")
      factorGraph.sendMsgs()
    }
    val marginalSupportSkills = factorGraph.getSupportSkillsMarginal()

    val skillMarginals = toSkillMarginals(priorSupportSkills.v, marginalSupportSkills, allVsSupportCov)
    skillMarginals
  }

  /**
   * Computing integral(p(u)*p(f|u)du)
   * @param u = support skills
   *
   * @return f - skill marginals
   */
  private def toSkillMarginals(priorSupportSkillsCov: Matrix, u: MultivariateGaussian, allVsSupportCov: Matrix): Seq[Gaussian] = {

    val Kfu = allVsSupportCov

    val A = Kfu * priorSupportSkillsCov.inv

    val marginalMu = A * u.m

    val marginalVar = sumRows((A * u.v) :* A)

    val marginalSkills = marginalMu.toArray.zip(marginalVar.toArray).map { case (mu, variance) => Gaussian(mu, variance) }

    marginalSkills
  }

}