package dk.tennis.compare.rating.multiskill.model.directpoint

import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import scala.collection._
import dk.tennis.compare.rating.multiskill.model.od.GenericOffenceDefenceModel

case class GenericDirectPointModel(player: String, multiSkillParams: MultiSkillParams) extends DirectPointModel {

  /**key - player*/
  val pointModels: mutable.Map[String, GenericOffenceDefenceModel] = mutable.Map()

  def processGame(game: Game) = {

    val opponent = if (game.player1.equals(player)) game.player2
    else if (game.player2.equals(player)) game.player1
    else throw new IllegalArgumentException("Player not found")

    val pointModel = pointModels.getOrElseUpdate(opponent, GenericOffenceDefenceModel(multiSkillParams))

    pointModel.processGame(game)
  }

   def pointProb(game:Game):Tuple2[Double, Double] = {
    val pointModel = pointModels.getOrElseUpdate(game.player2, GenericOffenceDefenceModel(multiSkillParams))

    pointModel.pointProb(game)
  }
}