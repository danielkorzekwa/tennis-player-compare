package dk.tennis.compare.rating.multiskill.analysis

import org.junit._
import Assert._
import dk.tennis.compare.rating.multiskill.domain.PlayerSkill
import dk.tennis.compare.rating.multiskill.domain.PlayerSkills

class TransitivityCheckTest {

  @Test def test {
    
    val playerSkills = 
      PlayerSkills("p1",PlayerSkill(0,1),PlayerSkill(0,1)) ::
      PlayerSkills("p1",PlayerSkill(0,1),PlayerSkill(0,1)) ::
      PlayerSkills("p1",PlayerSkill(0,1),PlayerSkill(0,1)) ::
      PlayerSkills("p1",PlayerSkill(0,1),PlayerSkill(0,1)) ::
      Nil
  }
}