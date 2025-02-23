package fapi

import java.util.UUID
import scala.util.Random

object Randy {
  def generateWithGpXp(from: Int, to: Int, fromXp: Int, toXp: Int, prevTotalDmgMin: Int, prevTotalDmgMax: Int): Randy = {

    val balanceMod: Double = (0.5 - Random.nextDouble()) * 0.25
    val rando: Double = (0.5 * Random.nextDouble()) * 0.25

    val factors: Seq[Double] = Random.shuffle(Seq(balanceMod, -balanceMod, rando))

    val price = from + (Random.nextInt(to - from) * factors(0)).toInt
    val plan = UpgradePlan.generateFixed()

    var nextDmgUpgrade = new DmgUpgrade(0)
    var nextGoldUpgrade = new GoldUpgrade(0)
    var nextExpUpgrade = new ExpUpgrade(0)
    var nextConsUpgrade = new ConsUpgrade(0)
    var nextChargeUpgrade = new ChargeUpgrade(0)
    var nextGene = plan
    var nextGold = price
    var upgradingdone = false
    while (!upgradingdone) {
      upgradingdone = true
      nextGene.next() match {
        case "DmgUpgrade" if nextDmgUpgrade.cost <= nextGold =>
          nextGold -= nextDmgUpgrade.cost
          nextDmgUpgrade = nextDmgUpgrade.upgrade()
          nextGene = nextGene.advance()
          upgradingdone = false
        case "ConsUpgrade" if nextConsUpgrade.cost <= nextGold =>
          nextGold -= nextConsUpgrade.cost
          nextConsUpgrade = nextConsUpgrade.upgrade()
          nextGene = nextGene.advance()
          upgradingdone = false
        case "ChargeUpgrade" if nextChargeUpgrade.cost <= nextGold =>
          nextGold -= nextChargeUpgrade.cost
          nextChargeUpgrade = nextChargeUpgrade.upgrade()
          nextGene = nextGene.advance()
          upgradingdone = false
        case "ExpUpgrade" if (nextExpUpgrade.cost <= nextGold) =>
          nextGold -= nextExpUpgrade.cost
          nextExpUpgrade = nextExpUpgrade.upgrade()
          nextGene = nextGene.advance()
          upgradingdone = false
        case "GoldUpgrade" if (nextGoldUpgrade.cost <= nextGold) =>
          nextGold -= nextGoldUpgrade.cost
          nextGoldUpgrade = nextGoldUpgrade.upgrade()
          nextGene = nextGene.advance()
          upgradingdone = false
        case _ => // can't afford planned upgrade yet
      }
    }

    val totalXp = fromXp + (Random.nextInt(toXp - fromXp) * (1.0+factors(1))).toInt
    val nextLvl = Player.getLvlFromTotalExp(totalXp)
    val nextExp = totalXp - Player.getCumulativeXpMap(nextLvl - 1)

    new Randy("rand_" + UUID.randomUUID().toString.substring(0, 8),
      nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade,
      nextGold,
      nextExp,
      nextLvl,
      Seq(),
      prevTotalDmgMin + (Random.nextInt(prevTotalDmgMax - prevTotalDmgMin) * (1.0+factors(2))).toInt,
      nextGene
    )
  }
}

@SerialVersionUID(1L)
class Randy(override val id: String,
            override val dmgUpgrade: DmgUpgrade,
            override val goldUpgrade: GoldUpgrade,
            override val expUpgrade: ExpUpgrade,
            override val consUpgrade: ConsUpgrade,
            override val chargeUpgrade: ChargeUpgrade,
            override val gold: Int,
            override val exp: Int,
            override val level: Int,
            override val history: Seq[(Round, Randy, Int, Int, Int, Seq[String])],
            override val dmgDoneTotal: Int,
            override val gene: UpgradePlan
            ) extends GenomePlayer(id, dmgUpgrade, goldUpgrade, expUpgrade, consUpgrade, chargeUpgrade, gold, exp, level, history, dmgDoneTotal, gene) {
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
    var nextHistoryUpgrade = historyBuffer
    var nextGene = gene
    var upgradingdone = false
    while (!upgradingdone) {
      upgradingdone = true
      nextGene.next() match {
        case "DmgUpgrade" if nextDmgUpgrade.cost <= nextGold =>
          nextGold -= nextDmgUpgrade.cost
          nextDmgUpgrade = nextDmgUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade.append("DmgUpgrade")
          upgradingdone = false
        case "ConsUpgrade" if nextConsUpgrade.cost <= nextGold =>
          nextGold -= nextConsUpgrade.cost
          nextConsUpgrade = nextConsUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade.append("ConsUpgrade")
          upgradingdone = false
        case "ChargeUpgrade" if nextChargeUpgrade.cost <= nextGold =>
          nextGold -= nextChargeUpgrade.cost
          nextChargeUpgrade = nextChargeUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade.append("ChargeUpgrade")
          upgradingdone = false
        case "ExpUpgrade" if (nextExpUpgrade.cost <= nextGold) =>
          nextGold -= nextExpUpgrade.cost
          nextExpUpgrade = nextExpUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade.append("ExpUpgrade")
          upgradingdone = false
        case "GoldUpgrade" if (nextGoldUpgrade.cost <= nextGold) =>
          nextGold -= nextGoldUpgrade.cost
          nextGoldUpgrade = nextGoldUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade.append("GoldUpgrade")
          upgradingdone = false
        case _ => // can't afford planned upgrade yet
      }
    }
    new Randy(id,
      nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade,
      nextGold,
      nextExp,
      nextLvl,
      //math.min(chargeCap, charges + 11),
      //nextPlanHits,
      history :+ (round, this, placement, rwdGold, rwdExp, nextHistoryUpgrade.toSeq),
      dmgDoneTotal + (if(!giveReward) 0 else getDamage(round)),
      nextGene
    )
  }

  def upgradePlayer(): Option[Randy] = {
      gene.next() match {
        case "DmgUpgrade" if dmgUpgrade.cost <= gold =>
          val upgraded = new Randy(id,
            dmgUpgrade.upgrade(), goldUpgrade, expUpgrade, consUpgrade, chargeUpgrade,
            gold - dmgUpgrade.cost,
            exp,
            level,
            history,
            dmgDoneTotal,
            gene.advance()
          )
          Some(upgraded.upgradePlayer().getOrElse(upgraded))
        case "ConsUpgrade" if consUpgrade.cost <= gold =>
          val upgraded = new Randy(id,
            dmgUpgrade, goldUpgrade, expUpgrade, consUpgrade.upgrade(), chargeUpgrade,
            gold - consUpgrade.cost,
            exp,
            level,
            history,
            dmgDoneTotal,
            gene.advance()
          )
          Some(upgraded.upgradePlayer().getOrElse(upgraded))
        case "ChargeUpgrade" if chargeUpgrade.cost <= gold =>
          val upgraded = new Randy(id,
            dmgUpgrade, goldUpgrade, expUpgrade, consUpgrade, chargeUpgrade.upgrade(),
            gold - chargeUpgrade.cost,
            exp,
            level,
            history,
            dmgDoneTotal,
            gene.advance()
          )
          Some(upgraded.upgradePlayer().getOrElse(upgraded))
        case "ExpUpgrade" if (expUpgrade.cost <= gold) =>
          val upgraded = new Randy(id,
            dmgUpgrade, goldUpgrade, expUpgrade.upgrade(), consUpgrade, chargeUpgrade,
            gold - expUpgrade.cost,
            exp,
            level,
            history,
            dmgDoneTotal,
            gene.advance()
          )
          Some(upgraded.upgradePlayer().getOrElse(upgraded))
        case "GoldUpgrade" if (goldUpgrade.cost <= gold) =>
          val upgraded = new Randy(id,
            dmgUpgrade, goldUpgrade.upgrade(), expUpgrade, consUpgrade, chargeUpgrade,
            gold - goldUpgrade.cost,
            exp,
            level,
            history,
            dmgDoneTotal,
            gene.advance()
          )
          Some(upgraded.upgradePlayer().getOrElse(upgraded))
        case _ => None // can't afford planned upgrade yet
    }
  }
}
