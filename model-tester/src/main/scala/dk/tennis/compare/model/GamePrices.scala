package dk.tennis.compare.model

import dk.tennis.compare.domain.BfMarket

case class GamePrices(player1: String, player2: String, p1Price: Double, p2Price: Double,bfMarket:BfMarket) {

  def getPrice(player: String): Double = {
    if (player1.equals(player)) p1Price
    else if (player2.equals(player)) p2Price
    else throw new IllegalArgumentException("Player not found: " + player)
  }
}
