package fapi;

object Upgrade {
  def canBuyNum(up: Upgrade[_], gold: Int): Int = {
    var i = 0
    var g = gold
    while (up.cost <= g) {
      g -= up.cost
      i += 1
    }
    i
  }
}

@SerialVersionUID(1L)
trait Upgrade[T <: Upgrade[T]] extends Serializable {
  def cost: Int
  def level: Int
  def upgrade(): T
}

@SerialVersionUID(1L)
class ExpUpgrade(val level: Int) extends Upgrade[ExpUpgrade] {
  override def cost: Int = math.floor((5.0+level)*(1.0+0.05*level)).toInt
  def upgrade(): ExpUpgrade = new ExpUpgrade(level + 1)
}

@SerialVersionUID(1L)
class GoldUpgrade(val level: Int) extends Upgrade[GoldUpgrade] {
  override def cost: Int = math.floor((5.0+level)*(1.0+0.05*level)*math.pow(1.01,level)).toInt
  def upgrade(): GoldUpgrade = new GoldUpgrade(level + 1)
  def roi(round: Round, placementTierGuess: Int = 8): Int = { // TODO fix for season 3
    val rwdGold = math.ceil(math.ceil(((10.0+round.tier)*(1.0+0.02*level)*math.pow(1.05, round.tier))*(1.0+(placementTierGuess-1.0)*0.05))*(1.0+round.boosts("gp"))).toInt
    val rwdGoldNext = math.ceil(math.ceil(((10.0+round.tier)*(1.0+0.02*(level+1))*math.pow(1.05, round.tier))*(1.0+(placementTierGuess-1.0)*0.05))*(1.0+round.boosts("gp"))).toInt
    math.ceil(cost / (rwdGoldNext - rwdGold + 0.1)).toInt
  }
  def worthIt(round: Round): Boolean = {
    val r = roi(round)
    r < (100 - round.num)
  }
}

@SerialVersionUID(1L)
class DmgUpgrade(val level: Int) extends Upgrade[DmgUpgrade] {
  override def cost: Int = math.floor((5.0+level)*(1.0+0.1*level)).toInt
  def upgrade(): DmgUpgrade = new DmgUpgrade(level + 1)
}

@SerialVersionUID(1L)
class ChargeUpgrade(val level: Int) extends Upgrade[ChargeUpgrade] {
  override def cost: Int = (math.pow(1.9,level)*10.0).toInt
  def upgrade(): ChargeUpgrade = new ChargeUpgrade(level + 1)
}

@SerialVersionUID(1L)
class ConsUpgrade(val level: Int) extends Upgrade[ConsUpgrade] {
  override def cost: Int = math.floor((25+25*level)*math.pow(1.6,level)).toInt
  def upgrade(): ConsUpgrade = new ConsUpgrade(level + 1)
}
