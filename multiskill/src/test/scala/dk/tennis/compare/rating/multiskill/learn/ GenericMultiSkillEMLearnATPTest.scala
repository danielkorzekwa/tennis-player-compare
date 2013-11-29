package dk.tennis.compare.rating.multiskill.learn

import org.junit._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import dk.atp.api.domain.SurfaceEnum.HARD
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.testutil.MultiSkillTestUtil._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import scala.util.Random
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader

class GenericMultiSkillEMLearnATPTest {

  private val logger = Logger(LoggerFactory.getLogger(getClass()))

  @Test def test_atp_results_2011 {

    val multiSkillParams = MultiSkillParams(
      skillOnServeTransVariance = 0.1,
      skillOnReturnTransVariance = 0.1,
      priorSkillOnServe = PlayerSkill(0, 1), priorSkillOnReturn = PlayerSkill(0, 1),
      perfVarianceOnServe = 100, perfVarianceOnReturn = 100)

//    val multiSkillParams = MultiSkillParams(
//    skillOnServeTransVariance = 0.1447976314041133,
//    skillOnReturnTransVariance = 0.09071895385735473,
//    priorSkillOnServe = PlayerSkill(3.9310534742642336, 3.671176735896532), priorSkillOnReturn = PlayerSkill(-2.5493943263356895, 2.6349872111975303),
//    perfVarianceOnServe = 200, perfVarianceOnReturn = 200)
    
    val atpFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
    val tournaments = MatchesLoader.loadTournaments(atpFile, 2006, 2011)

    val learnedParams = GenericMultiSkillEMLearn.learn(multiSkillParams, tournaments, maxIter = 30000, emProgress)

  }

  private def emProgress(emStatus: EMStatus) = logger.info(emStatus.toString)
}