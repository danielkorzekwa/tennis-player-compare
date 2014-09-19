package dk.tennis.compare.rating.multiskill.analysis

case class OnlineAvg {

  var total = 0d
  var count = 0

  def add(x: Double) {
    total += x
    count += 1
  }

  def getAvg(): Double = total / count

}