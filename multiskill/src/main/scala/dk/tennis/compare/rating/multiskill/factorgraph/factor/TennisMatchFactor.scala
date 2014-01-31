package dk.tennis.compare.rating.multiskill.factorgraph.factor

import dk.bayes.math.gaussian.Gaussian
import dk.bayes.math.gaussian.LinearGaussian
import dk.bayes.model.factor.GaussianFactor
import dk.bayes.model.factor.SingleTableFactor
import dk.bayes.model.factor.api.Factor
import dk.bayes.model.factor.api.SingleFactor
import dk.bayes.model.factor.api.TripleFactor
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.matchloader.MatchResult
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel
import dk.tennis.compare.rating.multiskill.model.multipoint.GenericMultiPointModel
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills
import dk.tennis.compare.rating.multiskill.model.od.Game
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.bayes.model.factor.BivariateGaussianFactor
import dk.tennis.compare.rating.multiskill.model.multipoint.PointsFactorGraph
import dk.tennis.compare.rating.multiskill.model.multipoint.PointsFactorGraph
import dk.tennis.compare.rating.multiskill.model.multipoint.PointsFactorGraph
import java.util.Date

case class TennisMatchFactor(p1Factor: PlayerFactor, p2Factor: PlayerFactor, outcomeVarId: Int,
  perfVarianceOnServe: Double, perfVarianceOnReturn: Double, game: Game) extends TripleFactor {

  private val p1VarId = p1Factor.playerVarId
  private val p2VarId = p2Factor.playerVarId

  private val ZERO_PROBABILITY = 1.0E-20

  private var p1OnServefactorGraph: PointsFactorGraph = _
  private var p2OnServefactorGraph: PointsFactorGraph = _

  def getVariableIds(): Seq[Int] = Vector(p1VarId, p2VarId, outcomeVarId)

  def marginal(varId: Int): SingleFactor = {
    val marginalFactor = varId match {

      case `p1VarId` => SkillsFactor(p1VarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case `p2VarId` => SkillsFactor(p2VarId, new Gaussian(0, Double.PositiveInfinity), new Gaussian(0, Double.PositiveInfinity))
      case `outcomeVarId` => SingleTableFactor(outcomeVarId, 2, Array(1, 1))
      case _ => throw new IllegalArgumentException("Unknown variable id: " + varId)
    }

    marginalFactor
  }

  def outgoingMessages(factor1: Factor, factor2: Factor, factor3: Factor): Tuple3[SkillsFactor, SkillsFactor, SingleTableFactor] = {
    outgoingMessagesInternal(factor1.asInstanceOf[SkillsFactor], factor2.asInstanceOf[SkillsFactor], factor3.asInstanceOf[SingleTableFactor])
  }

  def outgoingMessagesInternal(p1Skills: SkillsFactor, p2Skills: SkillsFactor, outcomeFactor: SingleTableFactor): Tuple3[SkillsFactor, SkillsFactor, SingleTableFactor] = {

    val initialP1Skills = toPlayerSkills(game.player1, p1Skills)
    val initialP2SKills = toPlayerSkills(game.player2, p2Skills)

    val model = GenericMultiPointModel(perfVarianceOnServe, perfVarianceOnReturn)
    val (newP1SkillOnServe, newP2SkillOnReturn, p1OnServefactorGraph) = model.skillMarginals(initialP1Skills.skillOnServe, initialP2SKills.skillOnReturn, game.p1PointsOnOffence._1, game.p1PointsOnOffence._2)
    val (newP2SkillOnServe, newP1SkillOnReturn, p2OnServefactorGraph) = model.skillMarginals(initialP2SKills.skillOnServe, initialP1Skills.skillOnReturn, game.p2PointsOnOffence._1, game.p2PointsOnOffence._2)
    this.p1OnServefactorGraph = p1OnServefactorGraph
    this.p2OnServefactorGraph = p2OnServefactorGraph

    val newP1Skills = PlayerSkills(initialP1Skills.player, new Date(0), newP1SkillOnServe, newP1SkillOnReturn)
    val newP2Skills = PlayerSkills(initialP2SKills.player, new Date(0), newP2SkillOnServe, newP2SkillOnReturn)

    val outcomeMsg = SingleTableFactor(outcomeVarId, 2, Array(1, 1))

    val p1SkillsMsg = toFactor(p1VarId, newP1Skills) / p1Skills
    val p2SkillsMsg = toFactor(p2VarId, newP2Skills) / p2Skills

    Tuple3(p1SkillsMsg, p2SkillsMsg, outcomeMsg)
  }

  /**
   * @return [perf marginals on serve, perf marginals on return]
   */
  def getPerfMarginals(): Tuple2[Seq[BivariateGaussianFactor], Seq[BivariateGaussianFactor]] = {

    val (p1OnServeP1WinPerfVar, p1OnServeP2LosePerfVar) = p1OnServefactorGraph.calcPerfMarginalsP1Wins()
    val (p1OnServeP2WinPerfVar, p1OnServeP1LosePerfVar) = p1OnServefactorGraph.calcPerfMarginalsP2Wins()

    val p1OnServePerfOnServe = List.fill(p1OnServefactorGraph.pointsWon)(p1OnServeP1WinPerfVar) ::: List.fill(p1OnServefactorGraph.allPoints - p1OnServefactorGraph.pointsWon)(p1OnServeP1LosePerfVar)
    val p1OnServePerfOnReturn = List.fill(p1OnServefactorGraph.allPoints - p1OnServefactorGraph.pointsWon)(p1OnServeP2WinPerfVar) ::: List.fill(p1OnServefactorGraph.pointsWon)(p1OnServeP2LosePerfVar)

    val (p2OnServeP1WinPerfVar, p2OnServeP2LosePerfVar) = p2OnServefactorGraph.calcPerfMarginalsP1Wins()
    val (p2OnServeP2WinPerfVar, p2OnServeP1LosePerfVar) = p2OnServefactorGraph.calcPerfMarginalsP2Wins()

    val p2OnServePerfOnServe = List.fill(p2OnServefactorGraph.pointsWon)(p2OnServeP1WinPerfVar) ::: List.fill(p2OnServefactorGraph.allPoints - p2OnServefactorGraph.pointsWon)(p2OnServeP1LosePerfVar)
    val p2OnServePerfOnReturn = List.fill(p2OnServefactorGraph.allPoints - p2OnServefactorGraph.pointsWon)(p2OnServeP2WinPerfVar) ::: List.fill(p2OnServefactorGraph.pointsWon)(p2OnServeP2LosePerfVar)

    Tuple2(p1OnServePerfOnServe ++ p2OnServePerfOnServe, p1OnServePerfOnReturn ++ p2OnServePerfOnReturn)
  }

  private def toFactor(varId: Int, playerSkills: PlayerSkills): SkillsFactor = {
    SkillsFactor(varId,
      Gaussian(playerSkills.skillOnServe.mean, playerSkills.skillOnServe.variance),
      Gaussian(playerSkills.skillOnReturn.mean, playerSkills.skillOnReturn.variance))
  }

  private def toPlayerSkills(player: String, skillsFactor: SkillsFactor): PlayerSkills = {
    val playerSkills = PlayerSkills(player, new Date(0),
      PlayerSkill(skillsFactor.skillOnServe.m, skillsFactor.skillOnServe.v),
      PlayerSkill(skillsFactor.skillOnReturn.m, skillsFactor.skillOnReturn.v))
    playerSkills
  }

}