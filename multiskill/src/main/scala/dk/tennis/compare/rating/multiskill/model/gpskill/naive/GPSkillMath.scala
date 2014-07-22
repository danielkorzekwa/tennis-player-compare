package dk.tennis.compare.rating.multiskill.model.gpskill.naive

import dk.bayes.math.gaussian.CanonicalGaussian
import dk.bayes.math.linear.Matrix
import dk.bayes.math.gaussian.MultivariateGaussian

object GPSkillMath {

  /**Updates skills = skills*gameMsg*/
  def updateProduct(skills: CanonicalGaussian, gameToSkillMsg: CanonicalGaussian, gameIndex: Int, mult: Boolean) {

    implicit class Update(x: Double) {
      def update(v: Double) = if (mult) x + v else x - v
    }

    val k00 = skills.k(gameIndex * 2, gameIndex * 2)
    skills.k.set(gameIndex * 2, gameIndex * 2, k00 update gameToSkillMsg.k(0, 0))

    val k01 = skills.k(gameIndex * 2, gameIndex * 2 + 1)
    skills.k.set(gameIndex * 2, gameIndex * 2 + 1, k01 update gameToSkillMsg.k(0, 1))

    val k10 = skills.k(gameIndex * 2 + 1, gameIndex * 2)
    skills.k.set(gameIndex * 2 + 1, gameIndex * 2, k10 update gameToSkillMsg.k(1, 0))

    val k11 = skills.k(gameIndex * 2 + 1, gameIndex * 2 + 1)
    skills.k.set(gameIndex * 2 + 1, gameIndex * 2 + 1, k11 update gameToSkillMsg.k(1, 1))

    val h0 = skills.h(gameIndex * 2)
    skills.h.set(gameIndex * 2, 0, h0 update gameToSkillMsg.h(0))

    val h1 = skills.h(gameIndex * 2 + 1)
    skills.h.set(gameIndex * 2 + 1, 0, h1 update gameToSkillMsg.h(1))

  }

  def getSkills(skills: MultivariateGaussian, gameIndex: Int): MultivariateGaussian = {
    val skillsMean = Matrix(skills.m(2 * gameIndex), skills.m(2 * gameIndex + 1))

    val skillsCov = Matrix(2, 2, Array(
      skills.v(2 * gameIndex, 2 * gameIndex), skills.v(2 * gameIndex, 2 * gameIndex + 1),
      skills.v(2 * gameIndex + 1, 2 * gameIndex), skills.v(2 * gameIndex + 1, 2 * gameIndex + 1)))

    MultivariateGaussian(skillsMean, skillsCov)
  }

}