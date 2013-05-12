package dk.tennis.compare.simulation.twopointsgame.player

trait PlayerSimulator {

  def simulate(playersNum:Int,skillMean:Double,skillVariance:Double):Seq[Player]
}