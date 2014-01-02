package dk.tennis.compare.rating.multiskill.model.od.utils

import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.tennis.compare.rating.multiskill.model.od.OffenceDefenceModel

object Predict {

  def predict(model: OffenceDefenceModel, games: Seq[Game]): Seq[Prediction] = {

    games.flatMap { g =>

      val player1Skills = model.getSkill(g.player1,g.player2)
      val player2Skills = model.getSkill(g.player2,g.player1)
      val (player1ProbOnServe, player2ProbOnServe) = model.pointProb(g)

      model.processGame(g)

      List(Prediction(g, g.player1, player1Skills.skillOnServe, g.player2, player2Skills.skillOnReturn, player1ProbOnServe, g.p1PointsOnOffence),
        Prediction(g, g.player2, player2Skills.skillOnServe, g.player1, player1Skills.skillOnReturn, player2ProbOnServe, g.p2PointsOnOffence))

    }

  }

}