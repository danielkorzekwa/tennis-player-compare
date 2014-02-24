package dk.tennis.compare

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.model.tournament.GenericTournamentModel
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.career.GenericCareerModel
import dk.tennis.compare.rating.multiskill.model.career.TournamentSkills
import dk.tennis.compare.rating.multiskill.model.career.CareerModelConfig
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.model.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.model.tournament.MatchSkills
import dk.tennis.compare.rating.multiskill.model.tournament.MatchSkills
import dk.tennis.compare.rating.multiskill.analysis.Calibrate
import dk.tennis.compare.rating.multiskill.analysis.LogLik
import dk.tennis.compare.rating.multiskill.model.tournament.TournamentModelConfig
import dk.tennis.compare.rating.multiskill.model.career.CareerModelConfig
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.multiskill.model.career.GPCareerModel
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import java.util.Date
import dk.tennis.compare.rating.multiskill.model.career.CorCareerModel
import dk.tennis.compare.rating.multiskill.model.career.TournamentSkillsCor

class CareerModelCorTest {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2013.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2009, 2011).take(2)

  @Test def test {
    println("start test")

    for (i <- 0 to 0) {

      val careerModelConfig = CareerModelConfig(
        initialSkillsOnServe = PlayerSkill(0, 0.8806472793221474), initialSkillsOnReturn = PlayerSkill(3997.9909513252546 - 4002.542974700307, 0.7527376376092434),
        skillOnServeTransVariance = 0.0005, skillOnReturnTransVariance = 0.0004,
        pointPerfVarianceOnServe = 102.61914136268837, pointPerfVarianceOnReturn = 102.61914136268837)

      val tournamentModelConfig = TournamentModelConfig(
        skillOnServeTransVariance = 0.1000000001, skillOnReturnTransVariance = 0.050000000100000004,
        pointPerfVarianceOnServe = 102.61914136268837, pointPerfVarianceOnReturn = 102.61913572928106)

      val (pointPerfVarianceOnServe, pointPerfVarianceOnReturn) = (195.61914136268837, 155)

      singleTest(careerModelConfig, tournamentModelConfig, (pointPerfVarianceOnServe, pointPerfVarianceOnReturn))
    }
  }

  private def singleTest(careerModelConfig: CareerModelConfig, tournamentModelConfig: TournamentModelConfig, matchModelConfig: Tuple2[Double, Double]) {

    val tournamentSkills: Seq[TournamentSkillsCor] = CorCareerModel(careerModelConfig).calcTournamentSkills(tournaments)

    val matchSkills = tournamentSkills.flatMap { t =>
      val initialPlayerSkills = t.initialPlayerSkills
      val tennisMatches = t.tournament.matchResults
      val matchSkills = GenericTournamentModel(tournamentModelConfig).calcMatchSkills(initialPlayerSkills, tennisMatches)
      matchSkills
    }

    // (winProb,pointsWon,pointsTotal)
    val predictions: Seq[Tuple3[Double, Int, Int]] = calcPredictions(matchSkills, matchModelConfig)

    //val calibration = Calibrate.calibrate(predictions, 100)
    //  calibration.sortBy(v => v._1).foreach(v =>
    //    println("%f,%f,%d".format(v._1, v._2, v._3)))

    //   println("Matches num: " + predictions.size / 2)
    println("Log lik: " + LogLik.logLik(predictions))

    // Print Roger Federer skills
    //    val playerSkillsOnServe = tournamentSkills.map { t =>
    //      val defaultSkills = PlayerSkills("RogerFederer", new Date(0), PlayerSkill(0, 1), PlayerSkill(0, 1))
    //      t.initialPlayerSkills.getOrElse("Roger Federer", defaultSkills).skillOnServe
    //    }
    //    val playerSkillsOnReturn = tournamentSkills.map { t =>
    //      val defaultSkills = PlayerSkills("RogerFederer", new Date(0), PlayerSkill(0, 1), PlayerSkill(0, 1))
    //      t.initialPlayerSkills.getOrElse("Roger Federer", defaultSkills).skillOnReturn
    //    }
    //    println("player skills on serve: ")
    //    playerSkillsOnServe.foreach(s => println(s.mean))
    //    println("player skills on return: ")
    //    playerSkillsOnReturn.foreach(s => println(s.mean))
  }

  private def calcPredictions(matchSkills: Seq[MatchSkills], matchModelConfig: Tuple2[Double, Double]): Seq[Tuple3[Double, Int, Int]] = {
    val predictions: Seq[Tuple3[Double, Int, Int]] = matchSkills.flatMap { m =>

      val genericPointModel = GenericPointModel(matchModelConfig._1, matchModelConfig._2)

      val p1PointProb = genericPointModel.pointProb(m.player1Skills.skillOnServe, m.player2Skills.skillOnReturn)
      val p2PointProb = genericPointModel.pointProb(m.player2Skills.skillOnServe, m.player1Skills.skillOnReturn)
      val p1MatchProb = matchProb(p1PointProb, p2PointProb, m.result.numOfSets)

      List(
        // match prob
        //   Tuple3(p1MatchProb, if (m.result.player1Won) 1 else 0, 1),
        //   Tuple3(1 - p1MatchProb, if (!m.result.player1Won) 1 else 0, 1))
        //point probm.
        Tuple3(p1PointProb, m.result.p1Stats.servicePointsWon, m.result.p1Stats.servicePointsTotal),
        Tuple3(p2PointProb, m.result.p2Stats.servicePointsWon, m.result.p2Stats.servicePointsTotal))
    }
    predictions
  }
  def matchProb(p1PointProb: Double, p2PointProb: Double, numOfSets: Int): Double = {

    val p1MatchProb = if (numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)
    p1MatchProb
  }

  //   private def trade(r: MatchResult, p1MatchProb: Double, exMatchProb: Option[Double]) {
  //    val outcome = if (r.player1Won) 1 else 0
  //
  //    val bankroll = (payout - stakes) + 1000
  //    val price = 1 / exMatchProb.get
  //    val stakePct = ((price - 1) * (p1MatchProb) - (1 - p1MatchProb)) / (price - 1)
  //    val stake = stakePct * bankroll * 0.2
  //    payout += (stake * (1d / exMatchProb.get) * outcome)
  //    stakes += stake
  //
  //    println("payout/stake/profit=%.2f/%.2f/%.2f".format(payout, stakes, payout - stakes))
  //  }
}