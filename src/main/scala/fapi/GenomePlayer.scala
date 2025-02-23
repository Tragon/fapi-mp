package fapi

import java.util.UUID
import scala.util.Random

class GenomePlayer (override val id: String,
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
                    val gene: UpgradePlan
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
    var nextGene = gene
    var upgradingdone = false
    while (!upgradingdone && gene.hasNext) {
      upgradingdone = true
      nextGene.next() match {
        case "DmgUpgrade" if nextDmgUpgrade.cost <= nextGold =>
          nextGold -= nextDmgUpgrade.cost
          nextDmgUpgrade = nextDmgUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade = nextHistoryUpgrade :+ "DmgUpgrade"
          upgradingdone = false
        case "ConsUpgrade" if nextConsUpgrade.cost <= nextGold =>
          nextGold -= nextConsUpgrade.cost
          nextConsUpgrade = nextConsUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade = nextHistoryUpgrade :+ "ConsUpgrade"
          upgradingdone = false
        case "ChargeUpgrade" if nextChargeUpgrade.cost <= nextGold =>
          nextGold -= nextChargeUpgrade.cost
          nextChargeUpgrade = nextChargeUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade = nextHistoryUpgrade :+ "ChargeUpgrade"
          upgradingdone = false
        case "ExpUpgrade" if (nextExpUpgrade.cost <= nextGold) =>
          nextGold -= nextExpUpgrade.cost
          nextExpUpgrade = nextExpUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade = nextHistoryUpgrade :+ "ExpUpgrade"
          upgradingdone = false
        case "GoldUpgrade" if (nextGoldUpgrade.cost <= nextGold) =>
          nextGold -= nextGoldUpgrade.cost
          nextGoldUpgrade = nextGoldUpgrade.upgrade()
          nextGene = nextGene.advance()
          nextHistoryUpgrade = nextHistoryUpgrade :+ "GoldUpgrade"
          upgradingdone = false
        case _ => // can't afford planned upgrade yet
      }
    }
    new GenomePlayer(id,
      nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade,
      nextGold,
      nextExp,
      nextLvl,
      //math.min(chargeCap, charges + 11),
      //nextPlanHits,
      history :+ (round, this, placement, rwdGold, rwdExp, nextHistoryUpgrade),
      dmgDoneTotal + (if(!giveReward) 0 else getDamage(round)),
      nextGene
    )
  }

  def mutate(factor: Double = 0.05): GenomePlayer = {
    val newGene = UpgradePlan.mutate(gene, factor)
    val baseName = id.split("_").head + "_" + "mut" + "_"
    new GenomePlayer(baseName + UUID.randomUUID().toString.replace("-", "").substring(0, 16 - baseName.length), dmgUpgrade, goldUpgrade, expUpgrade, consUpgrade, chargeUpgrade, gold, exp, level, history, dmgDoneTotal, newGene)
  }
}
