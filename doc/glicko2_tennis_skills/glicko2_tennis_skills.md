Modelling tennis skills on serve and return with Glicko 2 pairwise comparison model
===========================================================================

This document presents the Glicko 2 [1](#references) pairwise comparison method for estimating skills of tennis players both on serve and return.

Overview
---------------------------------------------------

Tennis skills are estimated from a temporal sequence of tennis match statistics. A single tennis match includes the following information:

* Unique id of player A
* Unique id of player B
* Total number of service points played by player A
* Total number of service points won by player A
* Total number of service points played by player B
* Total number of service points won by player B
* Date and time of a tennis match

Tennis skill is characterised by 4 variables:

* Rating - Tennis skill presented on interval scale [2](#references). 
* Variance - Amount of uncertainty in a player rating.
* Volatility - Player's consistency in a tennis match.
* Timestamp - Date and time, which tennis skill value refers to.

Player skills are updated in an iterative scheme. For every tennis match, The Glicko 2 function takes skills
for both players at the beginning of the match and calculates new skills at the end of the match.

![Tennis Glicko 2](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2.png "Tennis Glicko 2")

The following pictures illustrates calculation details for all tennis skills: Player A skill on serve, Player B skill on serve, Player A skill on return and Player B serve on return.

![Tennis Glicko 2 - Details](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2-details.png "Tennis Glicko 2 - Details")

Scala code example - Calculate new skill on serve for a tennis player
---------------------------------------------------

This example presents an update of player skill on serve. ([source code](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/src/test/scala/dk/tennis/compare/glicko2/Glicko2SingleUpdateTest.scala)).

	val df = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss")
	
	val ratingPlayerAOnServe = Rating(rating = 0.52, deviation = 0.27, volatility = 0.025,
	timestamp = df.parse("03-Mar-2012 13:30:00"))
	
	val ratingplayerBOnReturn = Rating(rating = 0.2, deviation = 0.21, volatility = 0.04,
	timestamp = df.parse("06-Mar-2012 16:00:00"))
	
	val newRatingPlayerAOnServe = GenericGlicko2Rating.newRating(
	ratingPlayerAOnServe, ratingplayerBOnReturn, score = 0.7,
	tau = 0.5, df.parse("15-Mar-2012 13:30:00"), discountDurationInDays = 7)
	
	assertEquals(0.528, newRatingPlayerAOnServe.rating, 0.001)
	assertEquals(0.269, newRatingPlayerAOnServe.deviation, 0.001)
	assertEquals(0.024, newRatingPlayerAOnServe.volatility, 0.001)

History of skills for Roger Federer and Novak Djokovic tennis players
--------------------------------------------------------------------------

Roger Federer generally performs better on serve than Novak Djokovic over the period of time since 2006 till 2011. He also presents consistent performance on return over those 5 years.
On the other side, Novak Djokovic continuously improves his skills on return, which in a consequence lifts him up to the first place in the ATP ranking in the middle of 2011. 
Moreover, thanks to mastering his skills on return, during the year 2011 he won all but one tennis matches against Roger Federer. 
At the end of year 2011, Novak Djokovic possessed the best tennis skills on return out of all tennis players.

![History of skills for Federer and Djokovic](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/federer_djokovic_skills_history.png "History of skills for Federer and Djokovic")

References
----------
1. Professor Mark E. Glickman. Glicko 2 Rating System
2. http://en.wikipedia.org/wiki/Level_of_measurement#Interval_scale







 
