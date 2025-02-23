package fapi

import util.CustomObjectInputStream

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass, PrintWriter}
import java.util.UUID
import javax.swing.LayoutStyle.ComponentPlacement
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Player {
  val norm = 0//10
  val vari = 0//40
  val gene = 500
  val rand = 1000
  val eger = 0//5
  val trag = 0//15

  val totalWeight = norm + vari + gene + rand + eger + trag
  val normWeight = norm.toDouble / totalWeight
  val variWeight = normWeight + vari.toDouble / totalWeight

  val geneWeight = variWeight + gene.toDouble / totalWeight
  val randWeight = geneWeight + rand.toDouble / totalWeight
  val egerWeight = randWeight + eger.toDouble / totalWeight
  val tragWeight = egerWeight + trag.toDouble / totalWeight

  def build(id: Int, upgradePlan: Seq[UpgradePlan] = Seq()): Player = {
    val rnd = Random.nextDouble()
    if (rnd < normWeight) {
      new Player(("norm_" + id + "_" + UUID.randomUUID().toString.substring(0, 8)).padTo(18, '0'),
        new DmgUpgrade(0), new GoldUpgrade(0), new ExpUpgrade(0), new ConsUpgrade(0), new ChargeUpgrade(0),
        0, 0, 0, Seq(), 0)
    } else if (rnd < variWeight) {
      new VariablePlayer(("vari_" + id + "_" + UUID.randomUUID().toString.substring(0, 8)).padTo(18, '0'),
        new DmgUpgrade(0), new GoldUpgrade(0), new ExpUpgrade(0), new ConsUpgrade(0), new ChargeUpgrade(0),
        0, 0, 0, Seq(), 0,
        Random.nextDouble() * 0.5, Random.nextDouble() * 0.5)
    } else if (rnd < geneWeight) {
      new GenomePlayer(("gene_" + id + "_" + UUID.randomUUID().toString.substring(0, 8)).padTo(18, '0'),
        new DmgUpgrade(0), new GoldUpgrade(0), new ExpUpgrade(0), new ConsUpgrade(0), new ChargeUpgrade(0),
        0, 0, 0, Seq(), 0,
        Random.shuffle(upgradePlan).headOption.map(p => UpgradePlan.mutate(p, math.pow(Random.nextDouble(), 3))).getOrElse(UpgradePlan.generate())
      )
    } else if (rnd < randWeight) {
      new Randy(("rand_" + id + "_" + UUID.randomUUID().toString.substring(0, 8)).padTo(18, '0'),
        new DmgUpgrade(0), new GoldUpgrade(0), new ExpUpgrade(0), new ConsUpgrade(0), new ChargeUpgrade(0),
        0, 0, 0, Seq(), 0,
        UpgradePlan.generateFixed()
      )
    } else if (rnd < egerWeight) {
      new GenomePlayer(("eger_" + id + "_" + UUID.randomUUID().toString.substring(0, 8)).padTo(18, '0'),
        new DmgUpgrade(0), new GoldUpgrade(0), new ExpUpgrade(0), new ConsUpgrade(0), new ChargeUpgrade(0),
        0, 0, 0, Seq(), 0,
        UpgradePlan.generate())
    } else {
      new Tragon(("trag_" + id + "_" + UUID.randomUUID().toString.substring(0, 8)).padTo(18, '0'),
        new DmgUpgrade(0), new GoldUpgrade(0), new ExpUpgrade(0), new ConsUpgrade(0), new ChargeUpgrade(0),
        0, 0, 0, Seq(), 0)
    }
  }
  def buildBatch(num: Int, bestPlan: Seq[UpgradePlan] = Seq()): Seq[Player] = (0 until num).map(i => build(i, bestPlan))

  def save(player: Player, filename: String): Unit = {
    if (new File(filename).exists()) return
    val oos = new ObjectOutputStream(new FileOutputStream(filename))
    oos.writeObject(player)
    oos.close()
  }

  def load(filename: String): Option[Player] = {
    try {
      val ois = new ObjectInputStream(new FileInputStream(filename))
      val player = ois.readObject().asInstanceOf[Player]
      ois.close()
      Some(player)
    } catch {
      case e: Exception =>
        println(s"Error loading player from $filename: ${e.getMessage}")
        forceLoad(filename)
    }
  }

  def forceLoad(filename: String): Option[Player] = {
    try {
      val file = new File(filename)
      val ois = new CustomObjectInputStream(file)
      val player = ois.readObject().asInstanceOf[Player]
      ois.close()
      Some(player)
    } catch {
      case e: Exception =>
        println(s"Error loading player from $filename: ${e.getMessage}")
        None
    }
  }

  lazy val lvlXpMap: Map[Int, Int] = (0 to 100).map(lvl => (lvl, math.floor((10+lvl*5)*math.pow(1.02,lvl)).toInt)).toMap
  lazy val getCumulativeXpMap: Map[Int, Int] = lvlXpMap.map { case (lvl, xp) => (lvl, lvlXpMap.filter(_._1 <= lvl).values.sum) }
  def getLvlFromTotalExp(exp: Int): Int = getCumulativeXpMap.filter(_._2 <= exp).keys.max + 1
}

@SerialVersionUID(1L)
class Player( val id: String,
              val dmgUpgrade: DmgUpgrade,
              val goldUpgrade: GoldUpgrade,
              val expUpgrade: ExpUpgrade,
              val consUpgrade: ConsUpgrade,
              val chargeUpgrade: ChargeUpgrade,
              val gold: Int,
              val exp: Int,
              val level: Int,
              //val charges: Int,
              //val planHits: Int,
              val history: Seq[(Round, Player, Int, Int, Int, Seq[String])],
              val dmgDoneTotal: Int
            ) extends Serializable {
  val chargeCap: Int = chargeUpgrade.level + 8 + math.floor(level / 25).toInt

  def finished(placement: Int, round: Round, nextRound: Round, giveReward: Boolean = true): Player = {
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
      }

      val xpGoal = 0.4 * xpFactor
      val gpGoal = xpGoal + 0.4 * gpFactor
      if (rnd < xpGoal && nextExpUpgrade.cost <= nextGold) {
        nextGold -= nextExpUpgrade.cost
        nextExpUpgrade = nextExpUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "ExpUpgrade"
      } else if (rnd < gpGoal && nextGoldUpgrade.cost <= nextGold) {
        nextGold -= nextGoldUpgrade.cost
        nextGoldUpgrade = nextGoldUpgrade.upgrade()
        nextHistoryUpgrade = nextHistoryUpgrade :+ "GoldUpgrade"
      } else {
        upgradingdone = true
      }
      /*
      val xpGoal = 0.2 * xpFactor
      val gpGoal = xpGoal + 0.2 * gpFactor
      if (rnd < xpGoal && nextExpUpgrade.cost <= nextGold) {
        nextGold -= nextExpUpgrade.cost
        nextExpUpgrade = nextExpUpgrade.upgrade()
      } else if (rnd < gpGoal && nextGoldUpgrade.cost <= nextGold) {
        nextGold -= nextGoldUpgrade.cost
        nextGoldUpgrade = nextGoldUpgrade.upgrade()
      } else if (rnd < 0.6 && nextConsUpgrade.cost <= nextGold) {
        nextGold -= nextConsUpgrade.cost
        nextConsUpgrade = nextConsUpgrade.upgrade()
      } else if (rnd < 0.8 && nextDmgUpgrade.cost <= nextGold) {
        nextGold -= nextDmgUpgrade.cost
        nextDmgUpgrade = nextDmgUpgrade.upgrade()
      } else if (nextChargeUpgrade.cost <= nextGold) {
        nextGold -= nextChargeUpgrade.cost
        nextChargeUpgrade = nextChargeUpgrade.upgrade()
      } else {
        upgradingdone = true
      }
      */
    }
    new Player(id,
      nextDmgUpgrade, nextGoldUpgrade, nextExpUpgrade, nextConsUpgrade, nextChargeUpgrade,
      nextGold,
      nextExp,
      nextLvl,
      //math.min(chargeCap, charges + 11),
      //nextPlanHits,
      history :+ (round, this, placement, rwdGold, rwdExp, nextHistoryUpgrade),
      dmgDoneTotal + (if (!giveReward) 0 else getDamage(round))
    )
  }

  def getGpReward(round: Round, placement: Int): Int = math.ceil(math.ceil(((10.0 + round.tier) * math.pow(1.015, goldUpgrade.level) * math.pow(1.05, round.tier)) * placementTier(placement)) * (1.0 + round.boosts("gp"))).toInt
  def getXpReward(round: Round, placement: Int): Int = math.ceil(math.ceil(((10.0 + round.tier) * math.pow(1.015, expUpgrade.level) * math.pow(1.05, round.tier)) * placementTier(placement)) * (1.0 + round.boosts("xp"))).toInt

  protected def getRequiredXp(lvl: Int): Int = math.floor((10+lvl*5)*math.pow(1.02,lvl)).toInt
  def getDamage(round: Round): Int = {
    val numHits = chargeCap // planHits
    (0 until numHits).foldLeft(0)((sum, hit) => {
      sum + math.ceil(math.ceil((20.0+(3*dmgUpgrade.level)+(hit*(consUpgrade.level+1)))*(1+round.boosts("dmg")))).toInt
    })
  }

  protected def tryUpgrade[T <: Upgrade[T]](upgrade: T, gold: Int): (T, Int) = {
    val remaining = gold - upgrade.cost
    if (remaining >= 0) (upgrade.upgrade(), remaining) else (upgrade, gold)
  }

  protected def placementTier(placement: Int): Double = {
    if (placement > 2500) 1.0
    else if (placement > 1000) 1.02
    else if (placement > 250) 1.03
    else if (placement > 50) 1.04
    else if (placement > 10) 1.05
    else if (placement > 3) 1.06
    else if (placement > 1) 1.08
    else if (placement > 0) 1.1
    else ???
  }

  override def toString: String = {
    //s"Player ${id.formatted("%05d")}:\t${dmgUpgrade.level}\t${goldUpgrade.level}\t${expUpgrade.level}\t${consUpgrade.level}\t${chargeUpgrade.level}\t$gold\t$exp\t$level\t$charges\t$planHits\t$dmgDoneTotal"
    s"Player ${id}:\t${dmgUpgrade.level}\t${goldUpgrade.level}\t${expUpgrade.level}\t${consUpgrade.level}\t${chargeUpgrade.level}\t$level\t$dmgDoneTotal"
  }

  def explainHistoryUpgrade(): Unit = {
    history.sliding(2).foreach {
      case Seq((prevRound, prevPlayer, prevPlace, prevRwdGold, prevRwdExp, _), (round, player, place, rwdGold, rwdExp, _)) =>
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

  lazy val historyBuffer: ListBuffer[String] = new ListBuffer().addAll(getHistoryUpgrade)
  def getHistoryUpgrade: Seq[String] = history.lastOption.map(_._6).getOrElse(Seq())

  lazy val baseGene: UpgradePlan = getBaseGene
  def getBaseGene: UpgradePlan = new UpgradePlan(getHistoryUpgrade)

  def exportHistoryCsv(): Unit = exportHistoryCsv(id)

  def exportHistoryCsv(fileName: String): Unit = try{
    val file = new File(s"history/${fileName}.csv")
    val writer = new PrintWriter(file)
    writer.write("Round,DmgUpgrade,ConsUpgrade,ExpUpgrade,GoldUpgrade,ChargeUpgrade,Gold,Level,Xp,DmgDoneTotal,Place,RwdGold,RwdExp,BossTier\n")
    writer.write(getHistoryString.mkString("\n"))
    writer.close()
  } catch {
    case e: Exception => println(s"Error exporting history for $id: ${e.getMessage}")
  }

  def getHistoryString: Seq[String] =
    history.map { case (round, player, place, rwdGold, rwdExp, _) =>
      s"${round.num},${player.dmgUpgrade.level},${player.consUpgrade.level},${player.expUpgrade.level},${player.goldUpgrade.level},${player.chargeUpgrade.level},${player.gold},${player.level},${player.exp},${player.dmgDoneTotal + player.getDamage(round)},$place,$rwdGold,$rwdExp,${round.tier}"
    }

  def getUpgradeRoundMap: Map[Int, Map[String, Int]] = history.map {
    case (prevRound, prevPlayer, prevPlace, prevRwdGold, prevRwdExp, _) =>
      val upgrades = Seq(
        ("DmgUpgrade", prevPlayer.dmgUpgrade.level),
        ("GoldUpgrade", prevPlayer.goldUpgrade.level),
        ("ExpUpgrade", prevPlayer.expUpgrade.level),
        ("ConsUpgrade", prevPlayer.consUpgrade.level),
        ("ChargeUpgrade", prevPlayer.chargeUpgrade.level)
      )

      prevRound.num -> upgrades.map { case (name, level) =>
        name -> level
      }.toMap
  }.toMap

  def samePlayer(other: Player): Boolean = {
    id.split("_").head == other.id.split("_").head &&
      (baseGene.plan == other.baseGene.plan || dmgDoneTotal == other.dmgDoneTotal)
    /*math.abs(dmgDoneTotal - other.dmgDoneTotal) < 100 &&
      //gold == other.gold &&
      //exp == other.exp &&
      level == other.level &&
      dmgUpgrade.level == other.dmgUpgrade.level &&
      goldUpgrade.level == other.goldUpgrade.level &&
      expUpgrade.level == other.expUpgrade.level &&
      consUpgrade.level == other.consUpgrade.level &&
      chargeUpgrade.level == other.chargeUpgrade.level*/
  }
}
