package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.testutil.TestUtil._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

class GenericMultiSkillEMLearnTest {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  @Test def test_empty_matches {

    val multiSkillParams = MultiSkillParams(
      skillOnServeTransVariance = 1,
      skillOnReturnTransVariance = 2,
      priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
      perfVariance = 3)

    val results = Nil

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, results, maxIter = 5)
    assertMultiSkillParams(learnedParams, multiSkillParams, 0.0001)
  }

  @Test def test_atp_results_2011 {

    val multiSkillParams = MultiSkillParams(
      skillOnServeTransVariance = 0.02,
      skillOnReturnTransVariance = 0.02,
      priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
      perfVariance = 200)

    val matchResults = loadTennisMatches(2011, 2011)
    println("Tennis matches=" + matchResults.size)

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, matchResults, maxIter = 5000, emProgress)

  }

  private def emProgress(emStatus: EMStatus) = logger.info(emStatus.toString)
}