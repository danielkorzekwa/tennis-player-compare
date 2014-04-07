package dk.tennis.compare.rating.multiskill.model.gpskill

trait CovFunc {

  def cov(playerSkill1:PlayerGPSkill,playerSkill2:PlayerGPSkill):Double
}