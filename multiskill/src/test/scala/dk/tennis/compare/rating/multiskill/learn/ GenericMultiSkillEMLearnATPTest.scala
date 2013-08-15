package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.sim.GenericMatchSim
import scala.util.Random

class GenericMultiSkillEMLearnATPTest {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  @Test def test_atp_results_2011 {

    val multiSkillParams = MultiSkillParams(
      skillOnServeTransVariance = 0.00026103154972190633,
      skillOnReturnTransVariance = 0.0006103154972158028,
      priorSkillOnServe = PlayerSkill(2.7100608379747073, 0.499569787325858), priorSkillOnReturn = PlayerSkill(-2.7100608379751296, 0.49956978732869395),
      perfVarianceOnServe = 100, perfVarianceOnReturn = 100)

    val matchResults = loadTennisMatches(2011, 2011)
    println("Tennis matches=" + matchResults.size)

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, matchResults, maxIter = 3, emProgress)

  }

  private def emProgress(emStatus: EMStatus) = logger.info(emStatus.toString)
}