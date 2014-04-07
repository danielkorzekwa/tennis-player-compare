package dk.tennis.compare.rating.multiskill.model.multipoint

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.model.pointcormodel.GenericPointCorModel
import dk.bayes.math.gaussian.Gaussian

class GenericMultiPointModelTest {

  @Test def test {

    val p1Skill = Gaussian(-1, 1)
    val p2Skill = Gaussian(0.5, 1.2)

    val p1PerfVariance = 190
    val p2PerfVariance = 170
    val model = GenericMultiPointModel(p1PerfVariance, p2PerfVariance)

    val (newP1Skill, newP2Skill, factorGraph) = model.skillMarginals(p1Skill, p2Skill, 7400, 10000)

    assertGaussian(Gaussian(5.0527, 0.0613), newP1Skill, 0.0001)
    assertGaussian(Gaussian(-6.7624, 0.0620), newP2Skill, 0.0001)

    println(new GenericPointModel(p1PerfVariance, p2PerfVariance).pointProb(newP1Skill, newP2Skill))

  }

  @Test def test_till_convergence {

    var p1Skill = Gaussian(-1, 1)
    var p2Skill = Gaussian(0.5, 1.2)

    val p1PerfVariance = 190
    val p2PerfVariance = 170
    val model = GenericMultiPointModel(p1PerfVariance, p2PerfVariance)

    for (i <- 1 to 100) {
      val (newP1Skill, newP2Skill, factorGraph) = model.skillMarginals(p1Skill, p2Skill, 74, 100)
      p1Skill = newP1Skill
      p2Skill = newP2Skill
      println(new GenericPointModel(p1PerfVariance, p2PerfVariance).pointProb(p1Skill, p2Skill))
    }

    assertGaussian(Gaussian(5.3987, 0.0611), p1Skill, 0.0001)
    assertGaussian(Gaussian(-6.7556, 0.0617), p2Skill, 0.0001)

  }

}