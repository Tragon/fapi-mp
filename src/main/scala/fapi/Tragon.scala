package fapi

import java.util.UUID
import scala.util.Random

object Tragon {
  val upgradeMap: Map[Int, Seq[String]] = Map(
    1 -> Seq("charge", "charge", "dmg", "dmg", "dmg"),
    2 -> Seq("cons", "dmg", "dmg"),
    3 -> Seq("dmg", "dmg", "dmg"),
    4 -> Seq("charge", "dmg"),
    5 -> Seq("dmg", "dmg", "gold", "gold", "gold"),
    6 -> Seq("charge", "exp", "exp"),
    7 -> Seq("cons", "dmg"),
    8 -> Seq("dmg", "dmg"),
    9 -> Seq("dmg", "dmg"),
    10 -> Seq("charge"),
    11 -> Seq("dmg", "gold"),
    12 -> Seq("cons", "dmg"),
    13 -> Seq("dmg", "dmg", "gold"),
    14 -> Seq("gold", "gold", "exp"),
    15 -> Seq("charge"),
    16 -> Seq("dmg", "dmg", "gold", "exp"),
    17 -> Seq("dmg", "dmg", "gold"),
    18 -> Seq("dmg", "dmg", "dmg", "gold", "gold", "gold"),
    19 -> Seq("exp"),
    20 -> Seq("charge"),
    21 -> Seq("dmg", "dmg", "gold", "gold", "exp"),
    22 -> Seq("cons", "exp", "exp", "exp"),
    23 -> Seq("dmg", "dmg", "gold", "exp"),
    24 -> Seq("dmg", "gold", "gold", "gold", "gold", "gold", "gold", "exp", "exp", "exp"),
    25 -> Seq("dmg", "dmg", "gold"),
    26 -> Seq("dmg", "dmg", "exp"),
    27 -> Seq("dmg", "dmg"),
    28 -> Seq(),
    29 -> Seq("charge"),
    30 -> Seq("dmg"),
    31 -> Seq("cons"),
    32 -> Seq("dmg", "dmg", "gold", "gold", "exp"),
    33 -> Seq("dmg", "gold", "gold", "gold", "exp"),
    34 -> Seq("dmg", "dmg", "dmg", "dmg", "gold"),
    35 -> Seq("dmg", "dmg", "gold", "gold", "exp"),
    36 -> Seq("dmg", "dmg", "exp"),
    37 -> Seq("dmg", "dmg"),
    38 -> Seq("dmg"),
    39 -> Seq("exp", "exp", "exp"),
    40 -> Seq("gold", "exp"),
    41 -> Seq("charge", "exp", "exp"),
    42 -> Seq("dmg", "dmg", "gold"),
    43 -> Seq("cons"),
    44 -> Seq("dmg", "dmg", "gold", "exp", "exp"),
    45 -> Seq("dmg", "dmg"),
    46 -> Seq("dmg", "gold", "exp"),
    47 -> Seq("dmg", "dmg"),
    48 -> Seq("dmg", "dmg", "gold"),
    49 -> Seq("dmg", "gold", "exp"),


    //6 -> Seq("charge"),
    //6 -> Seq("cons"),
  )

  def generateTragon(gold: Int, totalXp: Int): Randy = {
    val nextLvl = Player.getLvlFromTotalExp(totalXp)
    val nextExp = totalXp - Player.getCumulativeXpMap(nextLvl - 1)
    val upgrades: Seq[String] = upgradeMap.values.flatten.toSeq
    new Randy("trag_" + UUID.randomUUID().toString.substring(0, 8),
      new DmgUpgrade(upgrades.count(_ == "dmg")), new GoldUpgrade(upgrades.count(_ == "gold")), new ExpUpgrade(upgrades.count(_ == "exp")), new ConsUpgrade(upgrades.count(_ == "cons")), new ChargeUpgrade(upgrades.count(_ == "charge")),
      gold, nextExp, nextLvl, Seq(), 0, new UpgradePlan(Seq()))
  }

  def generateAdv(name: String, dmg: Int, gold: Int, exp: Int, cons: Int, charge: Int, gp: Int, totalXp: Int, prevTotalDmg: Int, plan: Seq[String] = Seq(), forceOrder: Option[(String, String)] = None): Randy = {
    val nextLvl = Player.getLvlFromTotalExp(totalXp)
    val nextExp = totalXp - Player.getCumulativeXpMap(nextLvl - 1)
    if (forceOrder.isDefined) {
      val (upgrade1, upgrade2) = forceOrder.get
      var upgradePlan = new UpgradePlan(plan ++ UpgradePlan.generateFixed(dmg = dmg, cons = cons, charge = charge, gp = gp, xp = exp).plan)
      while (upgradePlan.plan.indexOf(upgrade1) > upgradePlan.plan.indexOf(upgrade2)) {
        upgradePlan = new UpgradePlan(plan ++ UpgradePlan.generateFixed(dmg = dmg, cons = cons, charge = charge, gp = gp, xp = exp).plan)
      }
      new Randy(name + "_" + UUID.randomUUID().toString.replace("-", "").substring(0, 15 - name.length),
        new DmgUpgrade(dmg), new GoldUpgrade(gold), new ExpUpgrade(exp), new ConsUpgrade(cons), new ChargeUpgrade(charge),
        gp, nextExp, nextLvl, Seq(), prevTotalDmg, upgradePlan)
    } else
    new Randy(name + "_" + UUID.randomUUID().toString.replace("-", "").substring(0, 15 - name.length),
      new DmgUpgrade(dmg), new GoldUpgrade(gold), new ExpUpgrade(exp), new ConsUpgrade(cons), new ChargeUpgrade(charge),
      gp, nextExp, nextLvl, Seq(), prevTotalDmg, new UpgradePlan(plan ++ UpgradePlan.generateFixed(dmg = dmg, cons = cons, charge = charge, gp = gp, xp = exp).plan))
  }
}

@SerialVersionUID(1L)
class Tragon(override val id: String,
             override val dmgUpgrade: DmgUpgrade,
             override val goldUpgrade: GoldUpgrade,
             override val expUpgrade: ExpUpgrade,
             override val consUpgrade: ConsUpgrade,
             override val chargeUpgrade: ChargeUpgrade,
             override val gold: Int,
             override val exp: Int,
             override val level: Int,
             override val history: Seq[(Round, Player, Int, Int, Int, Seq[String])],
             override val dmgDoneTotal: Int
            ) extends EagerUpgrader(id, dmgUpgrade, goldUpgrade, expUpgrade, consUpgrade, chargeUpgrade, gold, exp, level, history, dmgDoneTotal) {
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
    val realBuys = Tragon.upgradeMap.get(round.num)
    realBuys match {
      case Some(upgrades) =>
        upgrades.foreach {
          case "dmg" =>
            nextGold -= nextDmgUpgrade.cost
            nextDmgUpgrade = nextDmgUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "DmgUpgrade"
          case "gold" =>
            nextGold -= nextGoldUpgrade.cost
            nextGoldUpgrade = nextGoldUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "GoldUpgrade"
          case "exp" =>
            nextGold -= nextExpUpgrade.cost
            nextExpUpgrade = nextExpUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "ExpUpgrade"
          case "cons" =>
            nextGold -= nextConsUpgrade.cost
            nextConsUpgrade = nextConsUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "ConsUpgrade"
          case "charge" =>
            nextGold -= nextChargeUpgrade.cost
            nextChargeUpgrade = nextChargeUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "ChargeUpgrade"
          case e => println(s"Unknown upgrade $e")
        }
        if (nextGold < 0) {
          // now we are not optimal anymore
          // println(s"Negative gold $nextGold")
        }
      case None =>
        var upgradingdone = false
        while (!upgradingdone) {
          val rnd = Random.nextDouble()
          val xpFactor = math.exp(-nextRound.num / 100.0)
          val gpFactor = math.exp(-nextRound.num / 100.0)
          val xpGoal = 0.4 * xpFactor
          val gpGoal = 0.4 //* gpFactor
          val currentDamage = nextRound.attack(nextLvl, nextDmgUpgrade, nextConsUpgrade, nextChargeUpgrade.level + 8).toDouble
          val dmgValue = (nextRound.attack(nextLvl, nextDmgUpgrade.upgrade(), nextConsUpgrade, nextChargeUpgrade.level + 8).toDouble - currentDamage) / nextDmgUpgrade.cost
          val consValue = (nextRound.attack(nextLvl, nextDmgUpgrade, nextConsUpgrade.upgrade(), nextChargeUpgrade.level + 8).toDouble - currentDamage) / nextConsUpgrade.cost
          val chargeValue = (nextRound.attack(nextLvl, nextDmgUpgrade, nextConsUpgrade, nextChargeUpgrade.level + 9).toDouble - currentDamage) / nextChargeUpgrade.cost
          if (((dmgValue >= consValue && dmgValue >= chargeValue && Random.nextDouble() < 0.9) || Random.nextDouble() > 0.9) && nextDmgUpgrade.cost <= nextGold) {
            nextGold -= nextDmgUpgrade.cost
            nextDmgUpgrade = nextDmgUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "DmgUpgrade"
          } else if (((consValue >= chargeValue && consValue >= dmgValue && Random.nextDouble() < 0.9) || Random.nextDouble() > 0.9) && nextConsUpgrade.cost <= nextGold) {
            nextGold -= nextConsUpgrade.cost
            nextConsUpgrade = nextConsUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "ConsUpgrade"
          } else if (((chargeValue >= consValue && chargeValue >= dmgValue && Random.nextDouble() < 0.9) || Random.nextDouble() > 0.9) && nextChargeUpgrade.cost <= nextGold) {
            nextGold -= nextChargeUpgrade.cost
            nextChargeUpgrade = nextChargeUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "ChargeUpgrade"
          } else if (nextGoldUpgrade.cost <= nextGold && nextGoldUpgrade.worthIt(nextRound) && rnd < gpGoal) {
            nextGold -= nextGoldUpgrade.cost
            nextGoldUpgrade = nextGoldUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "GoldUpgrade"
          } else if (nextExpUpgrade.cost <= nextGold && nextRound.num < 80 && rnd < xpGoal) {
            nextGold -= nextExpUpgrade.cost
            nextExpUpgrade = nextExpUpgrade.upgrade()
            nextHistoryUpgrade = nextHistoryUpgrade :+ "ExpUpgrade"
          } else {
            upgradingdone = true
          }
        }
    }
    new Tragon(id,
      nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade,
      nextGold,
      nextExp,
      nextLvl,
      //math.min(chargeCap, charges + 11),
      //nextPlanHits,
      history :+ (round, this, placement, rwdGold, rwdExp, nextHistoryUpgrade),
      dmgDoneTotal + (if(!giveReward) 0 else getDamage(round))
    )
  }

  override def explainHistoryUpgrade(): Unit = {
    history.drop(Tragon.upgradeMap.size).sliding(2).foreach {
      case Seq((prevRound, prevPlayer, _, _, _, _), (round, player, _, _, _, _)) =>
        val upgrades = Seq(
          ("DmgUpgrade", prevPlayer.dmgUpgrade.level, player.dmgUpgrade.level),
          ("GoldUpgrade", prevPlayer.goldUpgrade.level, player.goldUpgrade.level),
          ("ExpUpgrade", prevPlayer.expUpgrade.level, player.expUpgrade.level),
          ("ConsUpgrade", prevPlayer.consUpgrade.level, player.consUpgrade.level),
          ("ChargeUpgrade", prevPlayer.chargeUpgrade.level, player.chargeUpgrade.level)
        ).filter { case (_, oldLevel, newLevel) => oldLevel < newLevel }

        if (upgrades.nonEmpty) {
          //println(s"Round ${round.num}:")
          upgrades.foreach { case (name, oldLevel, newLevel) =>
            //println(s"  $name upgraded from level $oldLevel to level $newLevel")
            //println(s"  $name\t$oldLevel > $newLevel")
            println(s"${round.num}: $name\t$oldLevel > $newLevel")
          }
        }
      case _ => // Do nothing if there is no previous round
    }
  }
}
