package dk.tennis.compare

import org.junit._
import org.junit.Assert._
import dk.tennis.compare.rating.multiskill.matchloader.MatchesLoader
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import scala.collection._
import scala.math._
import dk.tennis.compare.rating.multiskill.model.od.GenericOffenceDefenceModel
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.model.od.utils.Calibrate
import dk.tennis.compare.rating.multiskill.model.od.utils.LogLik
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.tennis.compare.rating.multiskill.matchloader.TournamentResult
import dk.tennis.compare.model.ExPricesMatchModel
import dk.tennis.compare.domain.BfMarket
import scala.io.Source
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult

class CalibrationTest {

  val matchesFile = "./src/test/resources/atp_historical_data/match_data_2006_2011.csv"
  val tournaments = MatchesLoader.loadTournaments(matchesFile, 2006, 2011)

  val multiSkillParams = MultiSkillParams(
    //   skillOnServeTransVariance = 0.000000001,
    //   skillOnReturnTransVariance = 0.000000001,
    skillOnServeTransVariance = 0.1447976314041133,
    skillOnReturnTransVariance = 0.09071895385735473,
    priorSkillOnServe = PlayerSkill(3.9310534742642336, 3.671176735896532), priorSkillOnReturn = PlayerSkill(-2.5493943263356895, 2.6349872111975303),
    perfVarianceOnServe = 200, perfVarianceOnReturn = 200)

  val pointModel = GenericOffenceDefenceModel(multiSkillParams)
  // val pointModel = DbnODModel(multiSkillParams.priorSkillOnServe, multiSkillParams.priorSkillOnReturn, multiSkillParams.perfVarianceOnServe)
  // val pointModel = MultiDbnODModel(multiSkillParams.priorSkillOnServe, multiSkillParams.priorSkillOnReturn, multiSkillParams.perfVarianceOnServe, directSkillVariance = 0.5)

  val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011_less_than_30m_before_kick_off.csv")
  //val marketDataSource = Source.fromFile("./src/test/resources/betfair_data/betfair_data_tennis_mens_2010_2011_more_than_1h_before_kick_off.csv")
  val bfMarkets = BfMarket.fromCSV(marketDataSource.getLines().drop(1).toList)
  val exModel = ExPricesMatchModel(tournaments, bfMarkets)

  //trading
  var payout = 0d
  var stakes = 0d

  @Test def test {

    //Seq of [predicted_prob, points won, points total]
    val predictions = calcPredictions()

    val calibration = Calibrate.calibrate(predictions, 2)

    calibration.sortBy(v => v._1).foreach(v =>
      println("%f,%f,%d".format(v._1, v._2, v._3)))

    println("Matches num: " + predictions.size / 2)
    println("Log lik: " + LogLik.logLik(predictions))

    //predictions.map(p => (if (p._1 > 0.5) 1 else 0, p._2)).map(p => if (p._1 == p._2) 1 else 0).foreach(println(_))

  }

  private def calcPredictions(): Seq[Tuple3[Double, Int, Int]] = {

    val predictions: Seq[Tuple3[Double, Int, Int]] = tournaments.flatMap { t =>

      t.matchResults.flatMap { r =>

        val p1MatchNum = pointModel.getMatchesNum(r.player1)
        val p2MatchNum = pointModel.getMatchesNum(r.player2)

        val matchPredictions = if (p1MatchNum > 50 && p2MatchNum > 50) {
          //  pointModel.calibrate()
          //  println("processing tournament:" + t.tournamentTime)

          val (p1PointProb, p2PointProb) = pointModel.pointProb(r.player1, r.player2)
          val p1MatchProb = matchProb(p1PointProb, p2PointProb, r.numOfSets)
          val exMatchProb = exModel.gameProb(t, r)

          if (exMatchProb.isDefined) {
            //  println(p1MatchProb + "," + exMatchProb.get)
            //for table contingency pair test
          }

          if (r.containsPlayer("Novak Djokovic")) {
            // println(p1MatchProb + ":" + r.player1 + ":" + r.player2 + ":" + t.tournamentTime + "," + t.tournamentName)
          }

          List(
            // match prob
            Tuple3(p1MatchProb, if (r.player1Won) 1 else 0, 1),
            Tuple3(1 - p1MatchProb, if (!r.player1Won) 1 else 0, 1))
          //point prob
          //  Tuple3(p1PointProb, r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal),
          //  Tuple3(p2PointProb, r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal))

        } else Nil

        pointModel.processGame(toGame(t, r))

        matchPredictions

      }

    }
    predictions
  }

  private def matchProb(p1PointProb: Double, p2PointProb: Double, numOfSets: Int): Double = {

    val p1MatchProb = if (numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)
    p1MatchProb
  }
  private def toGame(t: TournamentResult, r: MatchResult): Game = {
    val p1PointsOnServe = Tuple2(r.p1Stats.servicePointsWon, r.p1Stats.servicePointsTotal)
    val p2PointsOnServe = Tuple2(r.p2Stats.servicePointsWon, r.p2Stats.servicePointsTotal)
    val pointGame = Game(t.tournamentTime, r.player1, r.player2, p1PointsOnServe, p2PointsOnServe)
    pointGame
  }

  private def trade(r: MatchResult, p1MatchProb: Double, exMatchProb: Option[Double]) {
    val outcome = if (r.player1Won) 1 else 0

    val bankroll = (payout - stakes) + 1000
    val price = 1 / exMatchProb.get
    val stakePct = ((price - 1) * (p1MatchProb) - (1 - p1MatchProb)) / (price - 1)
    val stake = stakePct * bankroll * 0.2
    payout += (stake * (1d / exMatchProb.get) * outcome)
    stakes += stake

    println("payout/stake/profit=%.2f/%.2f/%.2f".format(payout, stakes, payout - stakes))
  }

}