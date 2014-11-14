package dk.tennis.compare.rating.multiskill.analysis.h2h

import java.util.Date

case class Head2HeadStat(timestamp:Date,player1:String,player2:String,p1Won:Int,p2Won:Int)