package harvest

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

case class Harvest(gatherer: String, date: LocalDate, fruit: String, amount: Double)
case class FruitPrice(fruit: String, date: LocalDate, price: Double)

object HarvestProblem extends App{

  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd") //2020-01-01

  val harvestSource = scala.io.Source.fromFile(new File(this.getClass.getResource("harvest.csv").toURI))
  val harvestCsvData = harvestSource.getLines

  val priceSource = scala.io.Source.fromFile(new File(this.getClass.getResource("prices.csv").toURI))
  val priceCsvData = priceSource.getLines

  //Sample: List( Harvest(John,2020-01-01,apples,0.01), Harvest(John,2020-01-01,oranges,0.06) )
  val harvestCsv = harvestCsvData.map(line => line.split(",").map(_.trim))
    .toList.tail
    .map(arr => Harvest(arr(0), LocalDate.parse(arr(1), formatter), arr(2), arr(3).toDouble))
//  harvestCsv.take(2).foreach(println)

  //Sample: List( FruitPrice(apples,2020-01-02,497.41), FruitPrice(oranges,2020-01-02,813.7) )
  val priceCsv = priceCsvData.map(line => line.split(",").map(_.trim))
    .toList.tail
    .map(arr => FruitPrice(arr(0), LocalDate.parse(arr(1), formatter), arr(2).toDouble))
//  priceCsv.take(2).foreach(println)



  //  1.1 Who is best gatherer in terms of amount of fruits gathered every month?
  val a11 = harvestCsv.groupBy(_.date.getMonth).map(e => (e._1, e._2.groupBy(_.gatherer).map(e2 => (e2._1, e2._2.map(_.amount).sum))))
  println("\n 1.1 - best-gatherer (by month) : " + a11.map(e => (e._1, e._2.maxBy(_._2))) )


  //  1.2 Are there employees that are best at gathering some specific fruits
  val a12 = harvestCsv.groupBy(_.gatherer).map(e => (e._1, e._2.groupBy(_.fruit).map(e2 => (e2._1, e2._2.map(_.amount).sum))))
  println("\n 1.2 - employees good at picking specific fruits : " + a12.map(e => (e._1, e._2.maxBy(_._2))))


  //  2.1 What is your best-earning fruit(overall and by month)?
  val a21 = for{
    x <- harvestCsv
    y <- priceCsv  if (x.fruit == y.fruit) && (x.date.toString == y.date.minusDays(1).toString)
  } yield {
    (x.fruit, x.date.getMonth, x.amount * y.price)
  }
  val b21 = a21.groupBy(t => t._2).map(e => (e._1, e._2.groupBy(n => n._1))).map(e => (e._1, e._2.map(e2 => (e2._1, e2._2.map(_._3).sum))))
  val c21 = b21.map(e => (e._1, e._2.maxBy(_._2)))
  println("\n 2.1 a - best-earning fruit (by month): " + c21.map(e => (e._1, (e._2._1, e._2._2))))
  println(" 2.1 b - best-earning fruit (overall): " + a21.groupBy(t => t._1).map(e => (e._1, e._2.map(_._3).sum)).maxBy(_._2) )


  //  2.2 Which is your least profitable fruit(overall and by month)
  val a22 = for{
    x <- harvestCsv
    y <- priceCsv  if (x.fruit == y.fruit) && (x.date.toString == y.date.minusDays(1).toString)
  } yield {
    (x.fruit, x.date.getMonth, x.amount * y.price)
  }
  val b22 = a22.groupBy(t => t._2).map(e => (e._1, e._2.groupBy(n => n._1))).map(e => (e._1, e._2.map(e2 => (e2._1, e2._2.map(_._3).sum))))
  val c22 = b22.map(e => (e._1, e._2.minBy(_._2)))
  println("\n 2.2 a - least profitable fruit (by month): " + c22.map(e => (e._1, (e._2._1, e._2._2))))
  println(" 2.2 b - least profitable fruit (overall): " + a22.groupBy(t => t._1).map(e => (e._1, e._2.map(_._3).sum)).minBy(_._2) )


  //  3.1 Which gatherer contributed most to your income(by year and month)
  val a31 = for{
    x <- harvestCsv
    y <- priceCsv  if (x.fruit == y.fruit) && (x.date.toString == y.date.minusDays(1).toString)
  } yield {
    (x.gatherer, x.date.getMonth, x.amount * y.price)
  }
  val b31 = a31.groupBy(t => t._2).map(e => (e._1, e._2.groupBy(n => n._1))).map(e => (e._1, e._2.map(e2 => (e2._1, e2._2.map(_._3).sum))))
  val c31 = b31.map(e => (e._1, e._2.maxBy(_._2)))
  println("\n 3.1 a - gatherer contributed most to income (by month): " + c31.map(e => (e._1, (e._2._1, e._2._2))))
  println(" 3.1 b - gatherer contributed most to income (overall): " + a31.groupBy(t => t._1).map(e => (e._1, e._2.map(_._3).sum)).maxBy(_._2) )


  harvestSource.close()
  priceSource.close()






}
