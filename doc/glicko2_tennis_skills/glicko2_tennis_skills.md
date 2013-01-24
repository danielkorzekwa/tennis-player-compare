Modelling tennis skills on serve and return with Glicko 2 pairwise comparison model
===========================================================================

This document presents the Glicko 2 [1](#references) pairwise comparison method for estimating skills of tennis players both on serve and return.
It starts with an overview on Glicko2 Tennis model, followed by an example of Scala code for calculating skills of tennis players. Later on, 
it demonstrates three examples of practical application for tennis skills. The first one, is a study on historical skills of Roger Federer and Novak Djokovic. 
The second case, is about calculating probability of winning a point on serve and return at the beginning of the match by a tennis player. The last example, 
illustrates how to predict outcomes of tennis matches using skills of tennis players on serve and return. 
Finally, Glicko2 is discussed in a context of potential improvements in modelling skills of tennis players. 

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

The following picture illustrates calculation details for all tennis skills: Player A skill on serve, Player B skill on serve, Player A skill on return and Player B skill on return.

![Tennis Glicko 2 - Details](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2-details.png "Tennis Glicko 2 - Details")

Scala code example - Calculate new value of skill on serve for a tennis player
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

Analysis of skills for Roger Federer and Novak Djokovic since 2006 till 2011
--------------------------------------------------------------------------

Roger Federer generally performs better on serve than Novak Djokovic over the period of time since 2006 till 2011. He also presents consistent performance on return over those 5 years.
On the other side, Novak Djokovic continuously improves his skills on return, which, in a consequence lifts him up to the first place in the ATP ranking in the middle of 2011. 
Moreover, thanks to mastering his skills on return, he won all but one tennis matches against Roger Federer during the year 2011. 
At the end of year 2011, Novak Djokovic possessed the best tennis skills on return out of all tennis players.

![History of skills for Federer and Djokovic](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/federer_djokovic_skills_history.png "History of skills for Federer and Djokovic")

Resources for plotting this chart with a Gnuplot tool [3](#references):

* [Gnu plot script](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/glicko.gnu)
* [Data: Roger Federer skills on serve](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerARatingOnServe.dat)
* [Data: Roger Federer skills on return](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerARatingOnReturn.dat)
* [Data: Novak Djokovic skills on serve](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerBRatingOnServe.dat)
* [Data: Novak Djokovic skills on return](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerBRatingOnReturn.dat)
* [Data: Matches won by Roger Federer](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerAWon.dat)
* [Data: Matches won by Novak Djokovic](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerBWon.dat)

Modelling probability of winning a point on serve and return with Glicko2 Tennis Skills
---------------------------------------------------------------------------------------

Probability of winning a tennis point is modelled with a Logit function [4](#references), which is learned with Logistic Regression [5](#references) with the following predictor and target variables:

* Predictor variables: Skills of tennis players at the beginning of a tennis match
* Target variable: Ratio of points won serve by player A against player B

To calculate probability of winning a point on return by player A against player B, we simply use `1 - probability of winning a point on serve by player B against player A.`

It should be noted, that we might use other predictor features than just tennis skills to improve the accuracy of predicted probabilities, 
for example, weather conditions, indoor/outdoor or player's endurance.
Nevertheless, predicting the winner of tennis point just from tennis skills gives reasonable level of accuracy as presented at chart below.

![Correlation Point Probability](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/correlation_point_probability.png "Correlation Point Probability")



Predicting outcome of tennis match using Glicko2 Tennis SKills and Hierarchical Markov Chain
--------------------------------------------------------------------------------------------

References
----------
1. Professor Mark E. Glickman. Glicko 2 Rating System
2. Interval_scale - http://en.wikipedia.org/wiki/Level_of_measurement#Interval_scale
3. Gnuplot - http://www.gnuplot.info/
4. Logit function - http://en.wikipedia.org/wiki/Logit
5. Logistic regression - http://en.wikipedia.org/wiki/Logistic_regression







 
