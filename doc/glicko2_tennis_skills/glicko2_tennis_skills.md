Modelling tennis skills on serve and return with Glicko 2 pairwise comparison model
===========================================================================

This document presents the Glicko 2 [1](#references) pairwise comparison method for estimating skills of tennis players both on serve and return.

Tennis skills are estimated from a temporal sequence of tennis match statistics. A single tennis match includes the following information:

* Unique id of player A
* Unique id of player B
* Total number of service points played by player A
* Total number of service points won by player A
* Total number of service points played by player B
* Total number of service points won by player B

Tennis skill is characterised by 3 numbers:

* Rating - Tennis skill presented on interval scale [2](#references). 
* Variance - Amount of uncertainty in a player rating.
* Volatility - Player's consistency in a tennis match.

Tennis skills are updated in an iterative scheme. For every tennis match, The Glicko 2 function takes skills
for both players at the beginning of the match and calculates new skills at the end of the match.

![Tennis Glicko 2](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2.png "Tennis Glicko 2")

The following pictures illustrates calculation details for all tennis skills: Player A skill on serve, Player B skill on serve, Player A skill on return and Player B serve on return.

![Tennis Glicko 2 - Details](https://raw.github.com/danielkorzekwa/tennis-player-compare/master/doc/glicko2_tennis_skills/tennis-glicko2-details.png "Tennis Glicko 2 - Details")

References
----------
1. Professor Mark E. Glickman. Glicko 2 Rating System
2. http://en.wikipedia.org/wiki/Level_of_measurement#Interval_scale







 
