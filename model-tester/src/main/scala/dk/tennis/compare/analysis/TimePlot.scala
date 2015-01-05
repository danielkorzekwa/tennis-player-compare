package dk.tennis.compare.analysis

import breeze.plot.Figure
import breeze.plot._
import java.util.Date
import java.text.SimpleDateFormat
import java.text.NumberFormat
import java.text.FieldPosition
import java.text.ParsePosition
case class TimePlot(legend: Boolean) {

  val f = Figure()
  f.subplot(0).legend = legend

  def add(data: Seq[Tuple2[Date, Double]], label: String) {

    val x = data.map(d => d._1.getTime().toDouble / (1000L * 3600 * 24))
    val y = data.map(d => d._2)

    val df = new SimpleDateFormat("dd-MM-yyy")
    val nf = new NumberFormat {
      def format(number: Double, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = {

        toAppendTo.append(df.format(number * (1000L * 3600 * 24)))
        toAppendTo
      }
      def format(number: Long, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = {
        throw new UnsupportedOperationException("Not implemented")
      }

      def parse(source: String, parsePosition: ParsePosition): Number = {
        throw new UnsupportedOperationException("Not implemented")
      }
    }
    f.subplot(0).xaxis.setNumberFormatOverride(nf)
    f.subplot(0).ylim(0, 10)

    f.subplot(0) += plot(x, y, name = label)

  }
}