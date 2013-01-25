Modelling tennis skills on serve and return with Glicko 2 pairwise comparison model
===========================================================================

Daniel Korzekwa, January 2013

### Abstract

This document presents the Glicko 2 [1](#references) pairwise comparison method for estimating skills of tennis players, both on serve and return.
It starts with review of existing literature on modelling tennis skills. Next, it gives an overview on Glicko2 Tennis model, followed by an example of Scala code for calculating skills of tennis players. 
Later on, it demonstrates three examples of practical applications for tennis skills. The first one, is a study on historical skills of Roger Federer and Novak Djokovic. 
The second case, is about calculating probability of winning a point on serve and return at the beginning of a tennis match. The last example, 
illustrates how to predict outcomes of tennis matches using skills of tennis players on serve and return. 
Finally, Glicko2 is discussed in a context of potential improvements in modelling skills of tennis players. 

Literature review
------------------------------------

Most of existing literature on modelling tennis players and matches, present statistical approach for calculating skills on serve and return [2,3](#references)
.
For example, Paul K. Newton and Kamran Aslam present the following method for calculating the probability of winning a point by player 1 against player 2, 
using skills on serve and return [2](#references):

	P_s(P1|P2) = S_s(P1) - (S_r(P2) - S_r), where:
	
	P_s(P1|P2) - Probability of winning a point on serve by player 1 against player 2.
	S_s(P1) - Skill on serve for player 1. It is a ratio of points won on serve by player 1 against all his opponents.
	S_r(P2) -  Skill on return for player 2. It is a ratio of points won on return by player 2 against all his opponents.
	S_r -  Average skill on return. It is a ratio of points won on return by all tennis players.

This formula works reasonably well, assuming all players played against most of other players, like if they played in a league. However, in tennis, more can be learned about
players by following transitive connections between them. For example, consider the following temporal sequence of tennis matches:

	Time T1: Player 1 wins against player 2
	Time T2: Player 2 wins against player 3
 
Looking at the transitive connections between players 1,2 and 3, we might reason that player 1 has a good chance of winning against player 3.
One technique that works particularly  well in a such environment is known as pairwise comparison [4](#references).

Pairwise comparison models are well established in games, such as chess or multi-team computer online games, but they are not so popular in modelling skills of tennis players.
Mark E. Glickman in his paper 'Parameter estimation in large dynamic paired comparison experiments' [5] (#references) analysed tennis players with pairwise comparison model, but he didn't
attempt to model separately skills on serve and return.

The most known pairwise comparison models include:

* Elo  - Designed for Chess [6](#references)
* Glicko and Glicko 2 - Designed for Chess [1](#references)
* TrueSkill - Designed for multi-team computer online games [7](#references)

For more details about literature on modelling tennis matches, read this article (section Tennis Prediction Model):
(http://blog.danmachine.com/2012/02/on-probability-of-winning-tennis-match.html)

Overview of Glicko 2 Tennis Model 
---------------------------------------------------

Tennis skills are estimated from a temporal sequence of tennis match statistics. A single tennis match includes the following information:

* Unique id of player 1
* Unique id of player 2
* Total number of service points played by player 1
* Total number of service points won by player 1
* Total number of service points played by player 2
* Total number of service points won by player 2
* Date and time of a tennis match

Tennis skill is characterised by 4 variables:

* Rating - Tennis skill presented on interval scale [8](#references). 
* Variance - Amount of uncertainty in a player rating.
* Volatility - Player's consistency in a tennis match.
* Timestamp - Date and time, which tennis skill value refers to.

Player skills are updated in an iterative scheme. For every tennis match, The Glicko 2 function takes skills
for both players at the beginning of the match and calculates new skills at the end of the match.

![Tennis Glicko 2](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2.png "Tennis Glicko 2")

The following picture illustrates calculation details for all tennis skills: skills on serve and return for players 1 and 2.

![Tennis Glicko 2 - Details](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2-details.png "Tennis Glicko 2 - Details")

### Calculate new value of skill on serve in Scala ([source code](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/src/test/scala/dk/tennis/compare/glicko2/Glicko2SingleUpdateTest.scala)).

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
At the end of the year 2011, Novak Djokovic possessed the best tennis skills on return out of all tennis players.

![History of skills for Federer and Djokovic](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/federer_djokovic_skills_history.png "History of skills for Federer and Djokovic")

Modelling probability of winning a point
---------------------------------------------------------------------------------------

Probability of winning a tennis point on serve and return at the beginning of a tennis match, is modelled with a Logit function [9](#references), which is learned using Logistic Regression [10](#references) with the following predictor and target variables:

* Predictor variables: Skills of tennis players at the beginning of a tennis match
* Target variable: Ratio of points won on serve by player 1 against player 2 during a match

To calculate probability of winning a point on return by player 1 against player 2, we use:

	P_r(P1|P2) = 1 - P_s(P2|P1), where:
	
	P_r(P1|P2) - Probability of winning a point on return by player 1 against player 2.
	P_s(P2|P1) - Probability of winning a point on serve by player 2 against player 1.

It should be noted, that we might use other predictor features than just tennis skills to improve the accuracy of prediction model, 
for example, weather conditions, indoor/outdoor or player's endurance.
Nevertheless, predicting the winner of tennis point, just from tennis skills, gives reasonable level of accuracy as presented at the chart below.

![Correlation Point Probability](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/correlation_point_probability.png "Correlation Point Probability")

The predicted probability of winning a point on serve is taken from tennis skills at the beginning of a tennis match. Whereas, the average ratio of points won 
on serve is an arithmetic mean of ratios of points won on serve, for those matches, which correspond to a specific value of predicted probability of winning a point on serve with a resolution of 0.01.

For example, consider an example of calculating the data for a correlation [11](#references) chart, for a set of two tennis matches:

	Match 1, player 1 (predicted probability = 0.6, actual ratio of points won = 0.62)
	Match 1, player 2 (predicted probability = 0.7, actual ratio of points won = 0.71)
	Match 2, player 3 (predicted probability = 0.6, actual ratio of points won = 0.59)
	Match 2, player 4 (predicted probability = 0.7, actual ratio of points won = 0.68)

Then, the corresponding correlation data looks as follows:

	predicted	actual
	0.6 		0.605
	0.7 		0.695

There is an infinite number of models for predicting probability of winning a tennis point, which would produce exactly the same correlation value. However, 
in general, the model with higher correlation value is the better one. Inspecting other measures, such as Entropy [12](#references), 
Kullback Leibler divergence [13](#references) and Log Likelihood [14](#references), gives deeper insight into comparing two models with similar value of Pearson Correlation. 

Predicting outcome of tennis match
--------------------------------------------------------------------------------------------

The probability of winning a tennis match by player 1 against player 2 is calculated with a hierachical markov chain [15,16](#references). 
This model takes as inputs, the probabilities of winning a point on serve by both players and calculates probabilities of winning a game, set, tiebreak and match. 
Tristan J. Barnett [15](#references) shows that markov chain allows for computing probability of winning a tennis match from an arbitrary starting position,
given the probabilities of winning a point by both players are known at this stage.

The following example ([source code](https://github.com/danielkorzekwa/tennis-probability-calculator/blob/master/src/test/scala/dk/tennisprob/TennisProbFormulaCalcTest.scala)) 
presents how to calculate game, set, tiebreak and match probabilities in Scala with the [Tennis Probability Calculator] (https://github.com/danielkorzekwa/tennis-probability-calculator)

	//Player 1 probability of winning a point on serve = 0.7
	//Player 2 probability of winning a point on serve = 0.6
	
	//Game - Player 1 on serve
	assertEquals(0.9007, TennisProbFormulaCalc.gameProb(0.7), 0.0001)
	//Game - Player 2 on serve
	assertEquals(0.7357, TennisProbFormulaCalc.gameProb(0.6), 0.0001)
	
	//Set - Player 1 on serve
	assertEquals(0.7948, TennisProbFormulaCalc.setProb(0.7, 0.4), 0.0001)
	//Set - Player 2 on serve
	assertEquals(0.2051, TennisProbFormulaCalc.setProb(0.6, 0.3), 0.0001)
	
	//Tie break - Player 1 on serve
	assertEquals(0.66297, TennisProbFormulaCalc.tiebreakProb(0.7, 0.4), 0.0001)
	//Tie break - Player 2 on serve
	assertEquals(0.3370, TennisProbFormulaCalc.tiebreakProb(0.6, 0.3), 0.0001)
	
	//3 set match - Player 1 on serve
	assertEquals(0.8910, TennisProbFormulaCalc.matchProb(0.7, 0.4, THREE_SET_MATCH), 0.0001)
	//3 set match - Player 2 on serve
	assertEquals(0.10898, TennisProbFormulaCalc.matchProb(0.6, 0.3, THREE_SET_MATCH), 0.0001)
	
	//5 set match - Player 1 on serve
	assertEquals(0.93805, TennisProbFormulaCalc.matchProb(0.7, 0.4, FIVE_SET_MATCH), 0.0001)
	//5 set match - Player 2 on serve
	assertEquals(0.061948, TennisProbFormulaCalc.matchProb(0.6, 0.3, FIVE_SET_MATCH), 0.0001)
	
The picture below presents the accuracy of predicting outcomes of tennis matches, using hierarchical markov chain
 and probabilities of winning a point, calculated from tennis skills on serve and return.
 
 ![Correlation Match Probability](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/correlation_match_probability.png "Correlation Match Probability")
 
The correlation data between the predicted probability of winning a match and the ratio of matches won, is prepared similarly to how it was described above in a section 'Modelling probability of winning a point'.
 
Summary
-------------------------------

Glicko2 gives reasonable accuracy on modelling skills of tennis players and predicting probabilities of winning a point on serve and return. 
It is also simple to implement and very fast to run. Therefore, at the first glance, Glicko2 Tennis model is a strong candidate for a number of practical applications, 
including creating leader boards of tennis players and predicting outcomes of tennis matches. 
However, there are some potential improvements that could be made, in order to increase general quality of this model. 

First, tennis matches are played at three major surfaces, HARD, GRASS and CLAY. Shall we create a single  model or maybe having a separate model for every surface is a better choice? 
Glicko2 Tennis model assumes that all tennis matches are played in the same conditions and the only thing that varies is the performance of tennis players in a match.

Secondly, Glicko2 is an online model [17](#references). It maintains the current belief in tennis skills and updates it iteratively by processing results of tennis matches in a serial order. 
To understand, why this could be an issue, look at the following example.

	Time T1: Player 1 (skill=100) wins against player 2 (skill=150)
	Time T2: Glicko 2 raises skill of player 1 up and lowers skill of player 2 
	Time T3: Player 2 (skill=140) wins against player 3 (skill=200) 
	Time T4: Glicko2 raises skill for player 2 up and lowers skill of player 3

The fact, that skill for player 2 goes up at the time T3, should change our belief in his skill at the time T1, which in a consequence should impact the skill for player 1 after the time T1.
In the world of Hidden Markov Models this concept is known as smoothing [18](#references). The idea behind smoothing is that, the more we learn about the future, the more we are certain about the past.
This technique was applied for modelling chess players by Pierre Dangauthier, Ralf Herbrich, Tom Minka, and Thore Graepel in their paper 'TrueSkill Through Time: Revisiting the History of Chess' [19](#references).
Although, TrueSkill in general is an online learning model, it was shown that, it could be adapted for smoothing historical player skills as well. 

Alternative technique to Glicko 2, which addresses those two issues described above is a pairwise comparison model based on Dynamic Bayesian Networks. 
The prototype for this model is described here:

[Predicting outcomes of tennis matches with Dynamic Bayesian Networks](https://github.com/danielkorzekwa/bayes-scala#getting-started---learning-parameters-with-expectation-maximisation-in-unrolled-dynamic-bayesian-networks-from-incomplete-data--1)

Appendix A: Scripts and data for plotting charts
---------------------------

All charts are plotted with a Gnuplot tool [20](#references).

**History of skills on serve/return for Roger Federer and Rafael Nadal on the HARD surface**

* [Gnu plot script](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/glicko.gnu)
* [Data: Roger Federer skills on serve](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerARatingOnServe.dat)
* [Data: Roger Federer skills on return](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerARatingOnReturn.dat)
* [Data: Novak Djokovic skills on serve](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerBRatingOnServe.dat)
* [Data: Novak Djokovic skills on return](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerBRatingOnReturn.dat)
* [Data: Matches won by Roger Federer](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerAWon.dat)
* [Data: Matches won by Novak Djokovic](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_federer_djokovic_chart/playerBWon.dat)

**Correlation between predicted probability of winning a point on serve and average ratio of points won on serve**

* [Gnu plot script](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_correlation_point_probability/point_prob_accuracy.gnu)
* [Data: Probability of winning a point vs ratio of points won](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_correlation_point_probability/point_prob_accuracy.dat)

**Correlation between predicted probability of winning a match and average ratio of matches won**

* [Gnu plot script](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_correlation_match_probability/match_prob_accuracy.gnu)
* [Data: Probability of winning a match vs ratio of matches won](https://github.com/danielkorzekwa/tennis-player-compare/blob/master/doc/glicko2_tennis_skills/plotting_correlation_match_probability/match_prob_accuracy.dat)


References
----------
1. Professor Mark E. Glickman. Glicko and Glicko 2 Rating Systems - http://www.glicko.net/glicko.html
2. Paul K. Newton, Kamran Aslam. Monte Carlo Tennis: A Stochastic Markov Chain Model. Journal of Quantitative Analysis in Sports, 2009
3. Tristan Barnett and Stephen R. Clarke. Combining player statistics to predict outcomes of tennis matches. IMA journal of management mathematics 16(2), 2005
4. Pairwise Comparison - http://en.wikipedia.org/wiki/Pairwise_comparison
5. Mark E. Glickman. Parameter estimation in large dynamic paired comparison experiments, 1999
6. Elo rating system - http://en.wikipedia.org/wiki/Elo_rating_system
7. TrueSkill Rating System - http://research.microsoft.com/en-us/projects/trueskill/
8. Interval_scale - http://en.wikipedia.org/wiki/Level_of_measurement#Interval_scale
9. Logit function - http://en.wikipedia.org/wiki/Logit
10. Logistic regression - http://en.wikipedia.org/wiki/Logistic_regression
11. Correlation - http://en.wikipedia.org/wiki/Correlation_and_dependence
12. Entropy - http://en.wikipedia.org/wiki/Entropy
13. Kullback Leibler divergence - http://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
14. Log likelihood - http://en.wikipedia.org/wiki/Log_likelihood
15. Tristan J. Barnett. Mathematical Modelling In Hierarchical Games with specific reference to tennis, 2006
16. O'Malley, A. James (2008) "Probability Formulas and Statistical Analysis in Tennis," Journal of Quantitative Analysis in Sports: Vol. 4: Iss. 2, Article 15
17. Online algorithm - http://en.wikipedia.org/wiki/Online_algorithm
18. Hidden Markov Model (Smoothing) - http://en.wikipedia.org/wiki/Hidden_Markov_model#Smoothing
19. Pierre Dangauthier, Ralf Herbrich, Tom Minka, and Thore Graepel. TrueSkill Through Time: Revisiting the History of Chess, 2008
20. Gnuplot - http://www.gnuplot.info/
