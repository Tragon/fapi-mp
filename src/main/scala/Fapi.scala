import fapi._

import java.io.File
import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object Fapi {

  def main(args: Array[String]): Unit = {
    //calcPossibilities()
    //findPossibilities()
    explain(3)
    //println(UpgradePlan.getPlayUpgradeBases().permutations.size)
    simulate()

    //explainContinuation(startRound)
    //continuation(startRound)
    //explainContinuation(startRound)
  }

  val csvCutoff = 3000
  val roundMapsCutoff = 3000

  val startRound = 75
  val randomizeBoosts = true

  val continueWithWins = true

  val targetWins = 25000
  val targetAny = true // if true, stop when anyone gets to targetWins, if false, stop when the best player gets to targetWins

  val trackClones = true
  val keepXBestIndividual = 10 // X player clones ea
  val keepXBestPlayers = 50 // non clone
  val numSeededPlayers = 2500 // with whitelisted players, everyone can be seeded
  val numSeededMutations = 5
  val numRandomPlayers = 500
  val numRandomPlayersMax = 1000

  private def continuation(startRound: Int): Unit = {

    def genPlayers: Seq[Randy] = (0 to numRandomPlayers + Random.nextInt(numRandomPlayersMax - numRandomPlayers))
      //name, dmg, gold, exp, cons, charge, gp, totalXp)
      .map(_ => Randy.generateWithGpXp(45055, 56581, 32555, 43082, 322052, 384892)) :+
      Tragon.generateAdv("dsad", 83, 58, 55, 7, 10, 19, 38516, 360841) :+
      Tragon.generateAdv("abc", 85, 50, 45, 7, 10, 830, 37348, 375080) :+
      Tragon.generateAdv("Moo", 84, 40, 37, 7, 10, 230, 34602, 372611) :+
      Tragon.generateAdv("Hans", 83, 76, 40, 7, 10, 616, 32883, 347159) :+
      Tragon.generateAdv("Kero", 85, 41, 43, 7, 10, 697, 38215, 374161) :+
      Tragon.generateAdv("Davi", 84, 43, 46, 7, 10, 167, 38200, 372269) :+
      Tragon.generateAdv("Elsp", 83, 50, 46, 7, 10, 271, 39017, 366415) :+
      Tragon.generateAdv("Casj", 80, 52, 44, 7, 10, 5245, 37276, 349135) :+
      Tragon.generateAdv("Kiru", 87, 43, 28, 7, 10, 85, 34767, 366343) :+
      Tragon.generateAdv("Nak", 87, 38, 40, 7, 10, 871, 39426, 376687) :+
      Tragon.generateAdv("Sayz", 87, 40, 43, 7, 10, 45, 39499, 367124) :+
      Tragon.generateAdv("Shee", 85, 41, 47, 7, 10, 351, 38470, 371742) :+
      Tragon.generateAdv("Kara", 84, 62, 41, 7, 10, 6455, 36933, 319481) :+
      Tragon.generateAdv("neig", 84, 48, 24, 7, 10, 5832, 33013, 364958) :+
      Tragon.generateAdv("Megi", 87, 50, 50, 7, 10, 234, 42262, 320930) :+
      Tragon.generateAdv("Rei2", 85, 50, 56, 7, 10, 609, 43082, 306381) :+
      Tragon.generateAdv("Reid", 85, 50, 56, 7, 10, 609, 43082, 306381) :+
      Tragon.generateAdv("TrNo", 87, 44, 42, 7, 10, 1181, 38648, 382243, forceOrder = Some("ConsUpgrade", "ChargeUpgrade")) :+
      Tragon.generateAdv("TrYe", 87, 44, 42, 7, 10, 1181, 38648, 382243, plan = Seq("ChargeUpgrade"))

    def runSeason(startRound: Int = 1, preSeeded: Seq[Player]): Seq[Player] = {
      var round = new Round(startRound, Round.bonusMap(startRound - 1)._1, Round.bonusMap(startRound - 1)._2)
      var players = genPlayers ++ preSeeded
      val whitelist = preSeeded.map(_.id)

      // warm up (setup pre-buys for this round without giving rewards/damage yet)
      val res = executeRound(round, players, giveReward = false)
      players = res._2

      (startRound - 1 until 100).foreach( i=> {
        val res = executeRound(round, players, ignoreBoostMap = randomizeBoosts, whitelist = whitelist)
        round = res._1
        players = res._2
      })
      //players.head.history.map(_._1).map(r => "" + (r.num-1) + " -> ("+ r.tier + ", Map(\"dmg\" -> " + r.boosts("dmg") + ", \"xp\" -> " + r.boosts("xp")  + ", \"gp\" -> " + r.boosts("gp") + ")),").foreach(println)
      //println(players.sortBy(_.dmgDoneTotal).reverse.take(5).mkString("\n"))
      players
    }
    // player, wins, avgDmg
    var players: Seq[(Player, Int, Int)] =
      if (!continueWithWins) new File("players-continued").listFiles().flatMap(f => Player.load(f.getPath)).sortBy(_.dmgDoneTotal).reverse.take(25).map(p => (p, 0, 0))
      else new File("players-continued").listFiles().flatMap(f => Player.load(f.getPath).map(p => (p , f.getName.split("_").last.split("\\.").head.toInt ))).sortBy(_._1.dmgDoneTotal).reverse.take(25).map(p => (p._1, p._2, p._1.dmgDoneTotal))
    var playerMap: Map[String, Seq[(Player, Int, Int)]] = players.groupBy(_._1.id.split("_").head)
    var i = 1
    while(players.isEmpty || (targetAny && players.maxBy(_._2)._2 < targetWins) || (!targetAny && players.maxBy(_._1.dmgDoneTotal)._2 < targetWins)) {
    //(0 until 25000).foreach(i => {
      i += 1

      if (i % 1000 == 0) {
        val files = new File("players-continued").listFiles()
        players.filter(_._2 > 1000).foreach(p => {
          Player.save(p._1, "players-continued/" + p._1.id + "_" + p._2 + ".ser")
        })
        files.foreach(f => f.delete())
      }

      val preSeeded: Seq[Player] = Random.shuffle(players).take(numSeededPlayers).map(_._1).flatMap(_.history.find(_._1.num == startRound).map(_._2))
      val mutations: Seq[GenomePlayer] = preSeeded.flatMap {
        case genome: GenomePlayer => Some(genome.mutate(Random.nextDouble() * 0.1))
        case _ => None
      }.take(numSeededMutations)
      val seasonResult = runSeason(startRound, preSeeded ++ mutations)

      //players = (players ++ seasonResult.take(5)).sortBy(_.dmgDoneTotal).reverse.take(25)
      /*val newPlayers: Seq[Player] = (players.map(_._1) ++ seasonResult).foldLeft[Seq[Player]](Seq()){ (res, nextPlayer) =>
        val existing: Option[Player] = res.find(_.id == nextPlayer.id)
        if (existing.isEmpty) res :+ nextPlayer
        else if (existing.get.dmgDoneTotal > nextPlayer.dmgDoneTotal) res.filterNot(_.id == nextPlayer.id) :+ nextPlayer // Only keep the lower dmg player
        else res
      }.sortBy(_.dmgDoneTotal).reverse.take(keepXBestPlayers)

      players = newPlayers.map(p => {
        val existing = players.find(_._1.id == p.id)
        (p, existing.map(_._2).getOrElse(1) + (if(preSeeded.exists(_.id == p.id)) 1 else 0), existing.map(e => (e._3.toDouble + ((p.dmgDoneTotal.toDouble - e._3.toDouble) / e._2.toDouble)).toInt).getOrElse(p.dmgDoneTotal) )
      })*/

      val newPlayers2: Seq[(Player, Int, Int)] = (players ++ seasonResult.map(p => (p, 1, p.dmgDoneTotal))).filter(!_._1.id.startsWith("rand"))
        .foldLeft[Seq[(Player, Int, Int)]](Seq()) { (res, nextPlayer) =>
          res.find(_._1.id == nextPlayer._1.id) match {
            case None => res :+ nextPlayer
            case Some(existing) =>
              val wins: Double = nextPlayer._2 + existing._2
              val baseAvg: Double = if (existing._2 == 1) nextPlayer._3 else existing._3
              val newDmg: Double = if (existing._2 == 1) existing._3 else nextPlayer._3
              val avg = (baseAvg + ((newDmg - baseAvg) / wins)).toInt
              if (existing._1.dmgDoneTotal > nextPlayer._1.dmgDoneTotal) {
                res.filterNot(_._1.id == nextPlayer._1.id) :+ (nextPlayer._1, wins.toInt, avg)
              } else { // Only keep the lower dmg player
                res.filterNot(_._1.id == nextPlayer._1.id) :+ (existing._1, wins.toInt, avg)
              }
          }
        }.foldLeft[Seq[(Player, Int, Int)]](Seq()){ (res, nextPlayer) =>
          val same = res.find(_._1.samePlayer(nextPlayer._1))
          if (same.isDefined) {
            if (same.get._2 > nextPlayer._2) res else res.filterNot(_._1.id == same.get._1.id) :+ nextPlayer
          } else {
            res :+ nextPlayer
          }
        }
      if (trackClones) {
        players = newPlayers2.map(p => (p._1.id.split("_").head, p)).groupBy(_._1).flatMap(g => g._2.map(_._2).sortBy(_._1.dmgDoneTotal).reverse
          .foldLeft[Seq[(Player, Int, Int)]](Seq()){ (res, nextPlayer) =>
          if (res.exists(p => p._2 > nextPlayer._2 * 1.1)) res
          else res :+ nextPlayer
        }.take(keepXBestIndividual)).toSeq.sortBy(_._1.dmgDoneTotal)
      } else {
        // filter out players that have fewer than half wins than players with same or higher average damage total
        val newPlayers3 = newPlayers2.sortBy(_._3).reverse.foldLeft[Seq[(Player, Int, Int)]](Seq()){ (res, nextPlayer) =>
          if (res.exists(p => p._2 > nextPlayer._2 * 1.1)) res
          else res :+ nextPlayer
        }
        //players = newPlayers2.sortBy(_._1.dmgDoneTotal).reverse.take(keepXBestPlayers)
        players = newPlayers3.sortBy(_._3).reverse.take(keepXBestPlayers).sortBy(_._1.dmgDoneTotal)
      }

      if (i % 100 == 0) {
        print(i, players)
      }
    }//)

    print(i, players)

    val files = new File("players-continued").listFiles()
    files.foreach(f => f.delete())
    players.foreach(p => {
      Player.save(p._1, "players-continued/" + p._1.id + "_" + p._2 + ".ser")
    })
  }

  def print(season: Int, players: Seq[Any]): Unit = {
    println("Season " + season)
    println(players.mkString("\n"))
    println("---------------------------------------------")
  }

  def explain(num: Int): Unit = {
    val players: Seq[Player] = new File("players").listFiles().flatMap(f => Player.load(f.getPath))
    players.sortBy(_.dmgDoneTotal).reverse.take(num).foreach(player => {
      println("---------------------------")
      println(player.id + " - Total damage: " + player.dmgDoneTotal)
      player.exportHistoryCsv()
      player.explainHistoryUpgrade()
    })
  }

  def explainContinuation(num: Int): Unit = {

    val players: Seq[(Player, String)] = new File("players-continued").listFiles().flatMap(f => Player.load(f.getPath).map(p => (p, f.getName)))
    val plist = players.sortBy(_._1.dmgDoneTotal).reverse
    plist.filter(_._2.split("_").last.split("\\.").head.toInt >= csvCutoff).foreach(player => {
      player._1.exportHistoryCsv(player._1.dmgDoneTotal + "_" + player._2)
    })
    val histories: Seq[Seq[String]] = plist.map(_._1).filter(_.id.startsWith("Trag")).map {
      case randy: Randy => randy.history.find(_._1.num == num).map(_._2).map(_.gene.plan).getOrElse(randy.getHistoryUpgrade)
      case player: Player => player.getHistoryUpgrade
      case _ => ???
    }
    //val commonHistory = histories.head.takeWhile(h => histories.forall(_.contains(h)))
    //println("Common history:")
    //println(commonHistory.mkString("\n"))

    val roundMaps: Seq[Map[Int, Map[String, Int]]] = plist.filter(_._1.id.startsWith("Trag")).filter(_._2.split("_").last.split("\\.").head.toInt >= roundMapsCutoff).map(_._1.getUpgradeRoundMap)
    // get the minimum upgrade level for all players at each round
    val minRoundMap: Map[Int, Map[String, Int]] = roundMaps.foldLeft(Map[Int, Map[String, Int]]()) { (acc, next) =>
      next.foldLeft(acc) { (acc2, next2) =>
        val round = next2._1
        val upgrades = next2._2
        val existing = acc2.getOrElse(round, Map())
        val newUpgrades = upgrades.foldLeft(existing) { (acc3, next3) =>
          val upgrade = next3._1
          val level = next3._2
          val existingLevel = acc3.getOrElse(upgrade, Int.MaxValue)
          if (level < existingLevel) acc3 + (upgrade -> level)
          else acc3
        }
        acc2 + (round -> newUpgrades)
      }
    }
    minRoundMap.toSeq.sortBy(_._1).foreach(round => println(round._1 + ": " + round._2.toSeq.sortBy(_._1).map(u => u._1 + " -> " + u._2).mkString("\t")))


    val majorityHistory = getMostCommonHistory(histories, 5)

    /*
    // loses info about ordering and will result in mostly pure damage
    val transposed: Seq[Seq[String]] = histories.transpose
    val groupys: Seq[Map[String, Seq[String]]] = transposed.map(f => f.groupBy(identity))
    val majorityStart = groupys.map(g => {
      val b = g.maxBy(_._2.length)
      (b._1, b._2.length)
    }).takeWhile(_._2 > 15)
    */


    //val majorityHistories: Seq[Seq[String]] = histories.map(_.takeWhile(h => histories.count(_.contains(h)) > histories.length / 2))
    //val majorityHistory = majorityHistories.maxBy(_.length)
    //val majorityHistory = histories.head.takeWhile(h => histories.count(_.contains(h)) > histories.length / 2)
    println("Majority history:")
    println(majorityHistory.mkString("\n"))
    /*
    plist.filter(_.id.startsWith("Trag")).take(num).foreach(player => {
      println("---------------------------")
      println(player.id + " - Total damage: " + player.dmgDoneTotal)
      player.exportHistoryCsv()
      //player.explainHistoryUpgrade()
    })
    plist.filter(p => !p.id.startsWith("Trag")).take(num).foreach(player => {
      println("---------------------------")
      println(player.id + " - Total damage: " + player.dmgDoneTotal)
      player.exportHistoryCsv()
      //player.explainHistoryUpgrade()
    })
    */
  }

  def getMostCommonHistory(histories: Seq[Seq[String]], cutoff: Int): Seq[String] = {
    if (histories.length > cutoff) {
      val first: (String, Seq[String]) = histories.transpose.headOption.map(f => f.groupBy(identity)).map(_.maxBy(_._2.length)).getOrElse(("", Seq()))
      if (first._2.length > cutoff) {
        first._1 +: getMostCommonHistory(histories.filter(f => f.head == first._1).map(_.drop(1)), cutoff)
      } else {
        Seq()
      }
    } else {
      Seq()
    }
  }

  def simulate(): Unit = {
    val files = new File("players").listFiles()
    var players: Seq[Player] = files.flatMap(f => Player.load(f.getPath))
    var bestPlan: Seq[(Int, UpgradePlan)] = Seq()
    (0 until 10000).foreach(i => {
      println("Season " + i)
      //val seasonBatch: Future[Seq[Player]] = Future.sequence((0 until( 3)).map(_ => Future(runSeason()))).map(_.flatten)
      //val result = Await.result(seasonBatch.map(f => f.sortBy(_.dmgDoneTotal).reverse.take(5)), Duration.Inf)
      //players = (players ++ result.take(5)).sortBy(_.dmgDoneTotal).reverse.take(25)

      val plan = bestPlan.map(_._2)
      //val plan = bestPlan.map(p => UpgradePlan.mutate(p._2, 0.01))

      val seasonResult = runSeason(plan, numOfRounds = 120)//.sortBy(_.dmgDoneTotal).reverse
      //val bestGene = seasonResult.find(_.isInstanceOf[GenomePlayer]).map(p => (p.dmgDoneTotal, p.asInstanceOf[GenomePlayer].history.head._2.asInstanceOf[GenomePlayer].gene))
      //bestPlan = if (bestPlan.isEmpty || bestPlan.get._1 < bestPlan.get._1) bestGene else bestPlan
      //players = (players.drop(1) ++ seasonResult.take(5)).sortBy(_.dmgDoneTotal).reverse.take(25)
      players = (players ++ seasonResult.take(5)).sortBy(_.dmgDoneTotal).reverse.take(25)
      //bestPlan = Some(players.head.dmgDoneTotal, players.head.getBaseGene)
      //val optimizeRound = Random.nextInt(20) * 5
      bestPlan = findBestPlans(seasonResult)
      Future{
        if (i == 0) files.foreach(f => f.delete())
        players.foreach(p => {
          Player.save(p, "players/" + p.id + ".ser")
        })
        if (i % 5 == 0) {
          players.sortBy(_.dmgDoneTotal).reverse.take(3).foreach(player => {
            player.exportHistoryCsv()
          })
        }
      }
      println("Player type_random_id    :\tdmg\tgpl\txpl\tcon\tcha\tlvl\tdmgDoneTotal")
      println(players.mkString("\n"))
      //bestPlan.map(p => p._1 + ": " + p._2.plan.take(10).map(_.replace("Upgrade", "")).mkString(", ")).foreach(println)
      //bestPlan.map(p => p._1 + ": " + p._2.plan.take(100).map(_.take(2)).mkString(", ")).foreach(println)
      println("---------------------------------------------")
    })
  }

  def findBestPlans(players: Seq[Player]): Seq[(Int, UpgradePlan)] = {
    (1 to 20).map(_ * 5 - 1).flatMap(rnum => players.find(p => p.history.length > rnum && p.history(rnum)._3 == 1).map(p => (p.dmgDoneTotal, p.getBaseGene)))
  }

  def normal(): Unit = {
    var players: Seq[Player] = Seq()
    (0 until 100).foreach(i => {
      println("Season " + i)
      players = players ++ runSeason().sortBy(_.dmgDoneTotal).reverse.take(5)
      println("---------------------------------------------")
    })
    println(players.sortBy(_.dmgDoneTotal).reverse.take(25).mkString("\n"))
  }

  def runSeason(bestPlan: Seq[UpgradePlan] = Seq(), startRound: Int = 1, numOfRounds: Int = 100): Seq[Player] = {
    var round = new Round(startRound, Round.bonusMap(startRound - 1)._1, Round.bonusMap(startRound - 1)._2)
    var players = Player.buildBatch(8000, bestPlan)
    (startRound - 1 until numOfRounds).foreach( i=> {
      val res = executeRound(round, players)
      round = res._1
      players = res._2
    })
    //players.head.history.map(_._1).map(r => "" + (r.num-1) + " -> ("+ r.tier + ", Map(\"dmg\" -> " + r.boosts("dmg") + ", \"xp\" -> " + r.boosts("xp")  + ", \"gp\" -> " + r.boosts("gp") + ")),").foreach(println)
    //println(players.sortBy(_.dmgDoneTotal).reverse.take(5).mkString("\n"))
    players
  }

  def executeRound(round: Round, players: Seq[Player], giveReward: Boolean = true, ignoreTierMap: Boolean = false, ignoreBoostMap: Boolean = randomizeBoosts, whitelist: Seq[String] = Seq()): (Round, Seq[Player]) = {
    val dmgMap = players.map(p => (p, p.getDamage(round)))
    val totalDmg = dmgMap.map(_._2).sum
    val grouped = dmgMap.groupBy(_._2).toList.sortBy(_._1).reverse
    var newPlayers: ListBuffer[Player] = ListBuffer()
    val nextRound = round.next(totalDmg, ignoreTierMap, ignoreBoostMap)
    if (round.num == 5) {
      //grouped.headOption.map(_._2.size).foreach(println)
    } else if (round.num == 90) {
      //grouped.headOption.map(_._2.size).foreach(println)
    }
    grouped.foldLeft(1)((acc, group) => {
      val (_, players) = group
      newPlayers.addAll(players.map(p => p._1.finished(acc, round, nextRound, giveReward)))
      acc + players.count(p => !whitelist.contains(p._1.id))
    })
    //(nextRound, newPlayers.sortBy(_.dmgDoneTotal).reverse)
    (nextRound, newPlayers.toSeq)
  }

  val bossTier = 26
  val bonus = Map("dmg" -> 0.00, "xp" -> 0.05, "gp" -> 0.10)
  val pLvl = 23
  val pGp = 419
  val pXp = 44
  val dmgUp    = new DmgUpgrade(   27)
  val consUp   = new ConsUpgrade(   3)
  val expUp    = new ExpUpgrade(    4)
  val goldUp   = new GoldUpgrade(  12)
  val chargeUp = new ChargeUpgrade( 6)

  def calcPossibilities(): Unit = {
    val round = new Round(0, bossTier, bonus)
    val player: Randy = newRandy()
    val dmgPerms: Seq[Seq[String]] = possibleDmgUpgrades(player).permutations.toSeq
    val dmgBuffedPlayers: Seq[(Int, Randy)] = (dmgPerms.flatMap(perm => {
      val plan = new UpgradePlan(perm)
      newRandy(plan).upgradePlayer().map(p =>
        (p.getDamage(round), p)
      )
    }).toSeq :+ (player.getDamage(round), player))
      .groupBy(_._1).map(p => (p._1, p._2.map(_._2).sortBy(_.gold).reverse.head)).toSeq
      .sortBy(_._1).reverse
    println("Dmg possibilities")
    dmgBuffedPlayers.foreach(p => {
      println(p._1 + " - " + p._2.toString())
    })
    val mostDmgPlayer = dmgBuffedPlayers.head._2
  }

  def possibleDmgUpgrades(player: Player): Seq[String] =
    (0 until Upgrade.canBuyNum(player.dmgUpgrade, player.gold)).map(_ => "DmgUpgrade") ++
      (0 until Upgrade.canBuyNum(player.consUpgrade, player.gold)).map(_ => "ConsUpgrade") ++
      (0 until Upgrade.canBuyNum(player.chargeUpgrade, player.gold)).map(_ => "ChargeUpgrade")
  def possibleEcoUpgrades(player: Player): Seq[String] =
    (0 until Upgrade.canBuyNum(player.expUpgrade, player.gold)).map(_ => "ExpUpgrade") ++
      (0 until Upgrade.canBuyNum(player.goldUpgrade, player.gold)).map(_ => "GoldUpgrade")

  def ramdyPossibilities(): Unit = {
    val player = newRandy()
    val roundNum = 15
    var players: Seq[Player] = (0 until 32).map(_ => newRandy())
    var round = new Round(roundNum, 22, Round.bonusMap(roundNum)._2)
    (0 until 3).foreach( i=> {
      val res = executeRound(round, players)
      round = res._1
      players = res._2
    })
    players.map(p => (p.getDamage(round), p)).sortBy(_._1).reverse.foreach(p => {
      println(p._1 + " - " + p._2.id)
      p._2.getHistoryString.foreach(println)
      println()
    })
  }

  def newRandy(plan: UpgradePlan = new UpgradePlan(Seq())): Randy = new Randy("base", dmgUp, goldUp, expUp, consUp, chargeUp,
    pGp, pXp, pLvl,
    Seq(), 0, plan
  )
  def newPlayer(): Player = new Player("base", dmgUp, goldUp, expUp, consUp, chargeUp,
    pGp, pXp, pLvl,
    Seq(), 0)
}
