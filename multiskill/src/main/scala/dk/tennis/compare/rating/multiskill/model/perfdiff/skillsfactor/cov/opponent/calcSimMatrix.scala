package dk.tennis.compare.rating.multiskill.model.perfdiff.skillsfactor.cov.opponent

import dk.bayes.math.linear.Matrix
import scala.collection.immutable.IndexedSeq

object calcSimMatrix {

  def apply(players: IndexedSeq[String], skillsGivenOpponent: Map[String, Seq[PlayerSkill]], calcSimValue: (Array[Double],Array[Double]) => Double): Matrix = {

    def simMatrixValue(rowIndex: Int, colIndex: Int): Double = {

      val p1SkillsGivenOpponent = skillsGivenOpponent(players(rowIndex)).map(skill => skill.skill).toArray
      val p2SkillsGivenOpponent = skillsGivenOpponent(players(colIndex)).map(skill => skill.skill).toArray

      calcSimValue(p1SkillsGivenOpponent, p2SkillsGivenOpponent)
    }

    val simMatrix = Matrix(players.size, players.size, (rowIndex, colIndex) => simMatrixValue(rowIndex, colIndex))
    simMatrix
  }
}