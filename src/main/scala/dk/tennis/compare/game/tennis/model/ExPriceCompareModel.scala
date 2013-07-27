package dk.tennis.compare.game.tennis.model

import scala.util.Random

import org.slf4j.LoggerFactory

import dk.tennis.compare.tester.GameModel
import dk.tennis.compare.tester.GameResult

case class ExPriceCompareModel(exPricesModel: ExPricesMatchModel) extends GameModel {

  private val log = LoggerFactory.getLogger(getClass)

  private val trueSkillModel = TrueSkillMatchModel()
  private val multiSkillModel = MultiSkillModel()

  private val random = new Random()
  private var profit = 0d
  private var betsCount = 0

  def gameProb(r: GameResult): Option[Double] = {
    //     val trueSkillProb = trueSkillModel.gameProb(r)
    val gameProb = multiSkillModel.gameProb(r)
    val exProb = exPricesModel.gameProb(r)

    if (r.containsPlayer("Roger Federer")) {
     val skills = multiSkillModel.getMultiSkillModel.getSkills().get("Roger Federer")
   //  println(skills)
    }
    
    if (gameProb.isDefined) gameProb
    else None

  }

  def addGameResult(r: GameResult) {
    //        trueSkillModel.addGameResult(r)
    multiSkillModel.addGameResult(r)
  }

  private def trading(predictedProb: Double, exProb: Double, winner: Boolean) {

    val (prob, exProbVal, win) = if (random.nextBoolean) (predictedProb, exProb, winner)
    else (1 - predictedProb, 1 - exProb, !winner)

    val probDiff = (1 - prob / exProbVal)

    if (probDiff > 0.05) {
      val price = 1 / exProbVal
      val betProfit = if (win) (price - 1) else -1
      profit += betProfit
      betsCount += 1
    }
  }
}