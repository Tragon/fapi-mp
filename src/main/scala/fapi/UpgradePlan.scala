package fapi

import org.w3c.dom.css.Counter

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.util.Random

object UpgradePlan {
  def generateFixed(dmg: Int = 0, cons: Int = 0, charge: Int = 0, gp: Int = 0, xp: Int = 0): UpgradePlan = new UpgradePlan(
    Random.shuffle(getPlayUpgradeBases(dmg, cons, charge, gp, xp))
  )
  def generateFixed(): UpgradePlan = new UpgradePlan(
    Random.shuffle(getPlayUpgradeBases())
  )
  // [Seq[(Seq[String], Int)]]
  /*def generateWithBudget(from: Int, to: Int): (UpgradePlan, Int) = {
    val price = from + Random.nextInt(to - from)
    val plan: (Seq[String], Int) = Iterator.continually(getRndUpgrade)
      .foldLeft((Seq.empty[String], 0)) { case ((upgrades, totalCost), upgrade) =>
        val newTotalCost = totalCost + getUpgradeCost(upgrade, upgrades.count(_ == upgrade))
        if (newTotalCost <= to) (upgrades :+ upgrade, newTotalCost) else (upgrades, totalCost)
      }
      .takeWhile { case (_, totalCost) => totalCost < price }
      .last
    (new UpgradePlan(plan._1), plan._2)
  }
  def generateWithBudget(from: Int, to: Int): (UpgradePlan, Int) = {
    val price = from + Random.nextInt(to - from)
    val plan: (Seq[String], Int) = Iterator.continually(getRndUpgrade)
      .foldLeft[Seq[(Seq[String], Int)]](Seq(Seq(), 0)) { case ((upgrades, totalCost), upgrade) =>
        val newTotalCost = totalCost + getUpgradeCost(upgrade, upgrades.count(_ == upgrade))
        if (newTotalCost <= to) (upgrades :+ upgrade, newTotalCost) else (upgrades, totalCost)
      }
      .takeWhile { case (_, totalCost) => totalCost < price }
      .last
    (new UpgradePlan(plan._1), plan._2)
  }*/
  /*def generateWithBudgetAi(from: Int, to: Int): (UpgradePlan, Int) = {
    val price = from + Random.nextInt(to - from)
    val plan = Iterator.continually(getRndUpgrade)
      .scanLeft((Seq.empty[String], 0)) { case ((upgrades, totalCost), upgrade) =>
        val newTotalCost = totalCost + getUpgradeCost(upgrade, upgrades.count(_ == upgrade))
        if (newTotalCost <= to) (upgrades :+ upgrade, newTotalCost) else (upgrades, totalCost)
      }
      .dropWhile { case (_, totalCost) => totalCost < price }
      .next()
    (new UpgradePlan(plan._1), plan._2)
  }*/

  def getUpgradeCost(upgrade: String, counter: Int): Int = {
    upgrade match {
      case "ChargeUpgrade" => new ChargeUpgrade(counter).cost
      case "ConsUpgrade" => new ConsUpgrade(counter).cost
      case "DmgUpgrade" => new DmgUpgrade(counter).cost
      case "GoldUpgrade" => new GoldUpgrade(counter).cost
      case "ExpUpgrade" => new ExpUpgrade(counter).cost
    }
  }

  lazy val pregeneratedBases = getPlayUpgradeBases()
  def getPlayUpgradeBases(dmg: Int = 0, cons: Int = 0, charge: Int = 0, gp: Int = 0, xp: Int = 0): Seq[String] =
    (0 until 26 - charge).map(_ => "ChargeUpgrade") ++
      (0 until 20 - cons).map(_ => "ConsUpgrade") ++
      (0 until 250 - dmg).map(_ => "DmgUpgrade") ++
      (0 until 250 - gp).map(_ => "GoldUpgrade") ++
      (0 until 180 - xp).map(_ => "ExpUpgrade")
  def getPlayUpgradeBasesSeason1_2(dmg: Int = 0, cons: Int = 0, charge: Int = 0, gp: Int = 0, xp: Int = 0): Seq[String] =
    (0 until 13 - charge).map(_ => "ChargeUpgrade") ++
      (0 until 10 - cons).map(_ => "ConsUpgrade") ++
      (0 until 140 - dmg).map(_ => "DmgUpgrade") ++
      (0 until 80 - gp).map(_ => "GoldUpgrade") ++
      (0 until 80 - xp).map(_ => "ExpUpgrade")
  def generateRandom(num: Int = 1000): UpgradePlan = {
    val plan = (0 until num).map(_ => getRndUpgrade)
    new UpgradePlan(plan)
  }

  def generate(num: Int = 1000): UpgradePlan = generateFixed()
  def mutateSimple(plan: UpgradePlan, factor: Double): UpgradePlan = {
    val newPlan = plan.plan.flatMap { upgrade =>
      if (Random.nextDouble() < factor) {
        if (Random.nextDouble() < 0.5) None else Some(getRndUpgrade)
      } else Some(upgrade)
    }
    new UpgradePlan(newPlan ++ Seq.fill(plan.plan.length - newPlan.length)(getRndUpgrade))
  }
  def mutate(plan: UpgradePlan, factor: Double): UpgradePlan = {
    val newPlan = plan.plan.foldLeft[Seq[String]](Seq()) { (chain, upgrade) =>
      if (Random.nextDouble() < factor) {
        Random.nextInt(3) match {
          case 0 => chain // drop
          case 1 => chain :+ getRndUpgrade // mutate
          case 2 => chain :+ getRndUpgrade :+ upgrade // insert
        }
      } else chain :+ upgrade // keep
    }
    new UpgradePlan((newPlan ++ generateRandom(plan.plan.length - newPlan.length).plan).take(plan.plan.length))
  }

  def getRndUpgrade: String = getWeightedUpgrade

  def getWeightedUpgrade: String = pregeneratedBases(Random.nextInt(pregeneratedBases.length))
  def getEqualRndUpgrade: String = Random.nextInt(5) match {
    case 0 => "DmgUpgrade"
    case 1 => "GoldUpgrade"
    case 2 => "ExpUpgrade"
    case 3 => "ConsUpgrade"
    case 4 => "ChargeUpgrade"
  }
}

@SerialVersionUID(1L)
class UpgradePlan(val plan: Seq[String] = Seq()) extends Iterator[String] with Serializable {
  override def hasNext: Boolean = plan.nonEmpty
  override def next(): String = plan.headOption.getOrElse(UpgradePlan.getRndUpgrade)
  def advance(): UpgradePlan = if (plan.isEmpty) UpgradePlan.generate(100) else new UpgradePlan(plan.tail)
}
