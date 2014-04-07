package dk.tennis.compare.rating.multiskill.model.fastgpskill

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.Matrix
import dk.bayes.math.linear._
import dk.bayes.math.gaussian.MultivariateGaussian
import dk.tennis.compare.rating.multiskill.model.fastgpskill.factorgraph.GenericSkillsFactorGraph

case class GenericFastGpSkill( perfVarianceOnServe: Double, perfVarianceOnReturn: Double) extends FastGpSkill {

  def skillMarginals(players: Array[Player]): Seq[Gaussian] = {

    val supportPlayers = players.dropRight(2)

    val priorSupportSkillsMean = meanVector(supportPlayers)
    val priorSupportSkillsCov = covarianceMatrix(supportPlayers, supportPlayers)
    val priorSupportSkills = MultivariateGaussian(priorSupportSkillsMean, priorSupportSkillsCov)

    val allVsSupportCov = covarianceMatrix(players, supportPlayers)

    val factorGraph = GenericSkillsFactorGraph(priorSupportSkills, allVsSupportCov, players,perfVarianceOnServe,perfVarianceOnReturn)
    for (i <- 1 to 5) factorGraph.sendMsgs()
    val marginalSupportSkills = factorGraph.getSupportSkillsMarginal()

    val skillMarginals = toSkillMarginals(marginalSupportSkills, allVsSupportCov)
    skillMarginals
  }

  /**
   * Computing integral(p(u)*p(f|u)du)
   * @param u = support skills
   *
   * @return f - skill marginals
   */
  private def toSkillMarginals(u: MultivariateGaussian, allVsSupportCov: Matrix): Seq[Gaussian] = {

    val Kfu = allVsSupportCov

    val A = Kfu * u.v.inv

    val marginalMu = A * u.m

    val marginalVar = sumRows((A * u.v) :* A)

    val marginalSkills = marginalMu.toArray.zip(marginalVar.toArray).map { case (mu, variance) => Gaussian(mu, variance) }

    marginalSkills
  }

  private def meanVector(players: Array[Player]): Matrix = {
    Matrix.zeros(players.size, 1)
  }
  private def covarianceMatrix(playerRows: Array[Player], playerColumns: Array[Player]): Matrix = {
    Matrix(playerRows.size, playerColumns.size, (rowIndex, colIndex) => if (rowIndex == colIndex) 1d else 0.2)
  }

}