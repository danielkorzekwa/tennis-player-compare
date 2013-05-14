package dk.tennis.compare.game.twopointsgame.simulation

trait PlayerSimulator {

  def simulate(playersNum:Int,skillMean:Double,skillVariance:Double):Seq[Player]
}