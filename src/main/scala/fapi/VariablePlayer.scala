package fapi

import scala.util.Random

@SerialVersionUID(1L)
class VariablePlayer(override val id: String,
                     override val dmgUpgrade: DmgUpgrade,
                     override val goldUpgrade: GoldUpgrade,
                     override val expUpgrade: ExpUpgrade,
                     override val consUpgrade: ConsUpgrade,
                     override val chargeUpgrade: ChargeUpgrade,
                     override val gold: Int,
                     override val exp: Int,
                     override val level: Int,
                     override val history: Seq[(Round, Player, Int, Int, Int, Seq[String])],
                     override val dmgDoneTotal: Int,
                     val xpFactor: Double,
                     val gpFactor: Double,
                    ) extends Player(id, dmgUpgrade, goldUpgrade, expUpgrade, consUpgrade, chargeUpgrade, gold, exp, level, history, dmgDoneTotal) {
  override def finished(placement: Int, round: Round, nextRound: Round, giveReward: Boolean = true): Player = {
    val rwdGold = if (!giveReward) 0 else getGpReward(round, placement)
    val rwdExp = if (!giveReward) 0 else getXpReward(round, placement)
    //val minHits = (charges + 11) - chargeCap
    //val nextPlanHits = minHits + Random.nextInt(charges - minHits + 1)
    var nextGold = gold + rwdGold
    var nextExp = exp + rwdExp
    var nextLvl = level
    while (nextExp >= getRequiredXp(nextLvl)) {
      nextExp -= getRequiredXp(nextLvl)
      nextLvl += 1
    }
    var nextDmgUpgrade = dmgUpgrade
    var nextGoldUpgrade = goldUpgrade
    var nextExpUpgrade = expUpgrade
    var nextConsUpgrade = consUpgrade
    var nextChargeUpgrade = chargeUpgrade
    var nextHistoryUpgrade = getHistoryUpgrade
    //val potentialUpgrades = Seq(nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade).filter(_.cost <= nextGold * 5).flatMap(u => (0 until nextGold * 5 / u.cost).map(_ => u))
    var upgradingdone = false
    while (!upgradingdone) {
      val rnd = Random.nextDouble()
      //val xpFactor = 1.0 / (round.num / 80.0 + 1.0)
      //val gpFactor = 1.0 / (round.num / 100.0 + 1.0)
      val xpFactor = math.exp(-nextRound.num / 80.0)
      val gpFactor = math.exp(-nextRound.num / 100.0)
      val xpGoal = this.xpFactor * xpFactor
      val gpGoal = xpGoal + this.gpFactor * gpFactor
      val currentDamage = nextRound.attack(nextLvl, nextDmgUpgrade, nextConsUpgrade, nextChargeUpgrade.level + 8).toDouble
      val dmgValue = (nextRound.attack(nextLvl, nextDmgUpgrade.upgrade(), nextConsUpgrade, nextChargeUpgrade.level + 8).toDouble - currentDamage) / nextDmgUpgrade.cost
      val consValue = (nextRound.attack(nextLvl, nextDmgUpgrade, nextConsUpgrade.upgrade(), nextChargeUpgrade.level + 8).toDouble - currentDamage) / nextConsUpgrade.cost
      val chargeValue = (nextRound.attack(nextLvl, nextDmgUpgrade, nextConsUpgrade, nextChargeUpgrade.level + 9).toDouble - currentDamage) / nextChargeUpgrade.cost
      if (((dmgValue >= consValue && dmgValue >= chargeValue) || rnd > 0.9) && nextDmgUpgrade.cost <= nextGold) {
        nextGold -= nextDmgUpgrade.cost
        nextDmgUpgrade = nextDmgUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "DmgUpgrade"
      } else if (((consValue >= chargeValue && consValue >= dmgValue) || rnd > 0.9) && nextConsUpgrade.cost <= nextGold) {
        nextGold -= nextConsUpgrade.cost
        nextConsUpgrade = nextConsUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "ConsUpgrade"
      } else if (((chargeValue >= consValue && chargeValue >= dmgValue) || rnd > 0.9) && nextChargeUpgrade.cost <= nextGold) {
        nextGold -= nextChargeUpgrade.cost
        nextChargeUpgrade = nextChargeUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "ChargeUpgrade"
      } else if (nextExpUpgrade.cost <= nextGold && rnd < xpGoal) {
        nextGold -= nextExpUpgrade.cost
        nextExpUpgrade = nextExpUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "ExpUpgrade"
      } else if (nextGoldUpgrade.cost <= nextGold && nextGoldUpgrade.worthIt(nextRound) && rnd < gpGoal) {
        nextGold -= nextGoldUpgrade.cost
        nextGoldUpgrade = nextGoldUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "GoldUpgrade"
      } else {
        upgradingdone = true
      }
    }
    new VariablePlayer(id,
      nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade,
      nextGold,
      nextExp,
      nextLvl,
      //math.min(chargeCap, charges + 11),
      //nextPlanHits,
      history :+ (round, this, placement, rwdGold, rwdExp, nextHistoryUpgrade),
      dmgDoneTotal + (if(!giveReward) 0 else getDamage(round)),
      xpFactor,
      gpFactor
    )
  }
}
