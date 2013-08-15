package dk.tennis.compare.game.tennis.model

import dk.tennis.compare.tester.GameModel
import scala.math._
import dk.tennis.compare.tester.GameResult
import dk.tennis.compare.game.tennis.domain.TennisResult
import dk.tennisprob.TennisProbFormulaCalc
import dk.tennisprob.TennisProbCalc.MatchTypeEnum._
import dk.tennis.compare.rating.trueskill.model.TrueSkillRating
import dk.tennis.compare.rating.multiskill.GenericMultiSkill
import dk.tennis.compare.rating.trueskill.matchprob.GenericTrueSkillMatchProb
import dk.tennis.compare.rating.multiskill.domain.PointResult
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.pointmodel.GenericPointModel
import dk.tennis.compare.rating.multiskill.MultiSkill
import dk.tennis.compare.rating.multiskill.domain.MatchResult
import dk.tennis.compare.rating.multiskill.domain.MultiSkillParams
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill

case class MultiSkillModel extends GameModel {

   val multiSkillParams = MultiSkillParams(
    skillOnServeTransVariance = 2.625544365461093E-4,
    skillOnReturnTransVariance = 6.153475851415649E-4,
    priorSkillOnServe = PlayerSkill(2.453554354206633,1.3362242369052089), priorSkillOnReturn = PlayerSkill(-2.431379572976557,1.1378014945808683),
    perfVarianceOnServe = 100, perfVarianceOnReturn = 100)

  //     val multiSkillParams = MultiSkillParams(
  //    skillOnServeTransVariance = 0.03,
  //    skillOnReturnTransVariance = 0.03,
  //    priorSkillOnServe = PlayerSkill(0,1), priorSkillOnReturn = PlayerSkill(0,1),
  //    perfVariance = 200)

  private val multiSkillModel = GenericMultiSkill(multiSkillParams)

  def gameProb(r: GameResult): Option[Double] = {
    val player1Skill = multiSkillModel.getSkill(r.player1)
    val player2Skill = multiSkillModel.getSkill(r.player2)

    val pointModel = GenericPointModel(multiSkillModel.multiSkillParams.perfVarianceOnServe, multiSkillModel.multiSkillParams.perfVarianceOnReturn)
    val p1PointProb = pointModel.pointProb(player1Skill.skillOnServe, player2Skill.skillOnReturn)
    val p2PointProb = pointModel.pointProb(player2Skill.skillOnServe, player1Skill.skillOnReturn)

    val matchProb = if (r.asInstanceOf[TennisResult].numOfSets == 2) TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, THREE_SET_MATCH)
    else TennisProbFormulaCalc.matchProb(p1PointProb, 1 - p2PointProb, FIVE_SET_MATCH)

    Some(matchProb)

  }

  def addGameResult(r: GameResult) = {

    r match {
      case r: TennisResult => {

        val pointResults = r.points.get.map { point =>
          if (point.playerOnServe.equals(r.player1))
            PointResult(r.player1, point.won)
          else if (point.playerOnServe.equals(r.player2))
            PointResult(r.player2, point.won)
          else throw new IllegalArgumentException("Player on serve not found")
        }

        val matchResult = MatchResult(r.player1, r.player2, pointResults, r.player1Win.get, r.numOfSets)
        multiSkillModel.processTennisMatch(matchResult)

      }
      case _ => new IllegalArgumentException("Result type not supported:" + r)
    }
  }

  def getMultiSkillModel(): MultiSkill = multiSkillModel
}