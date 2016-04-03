package com.example

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

trait CappuccinoFutureSpec extends CappuccinoSpec[Some[String], Future[String]]

object FutureCappuccino extends CappuccinoFutureSpec {
  def grind(beans: CoffeeBeans) = Future {
    println("Starting grinder")
    Thread.sleep(Random.nextInt(2000))
    println("Stopped grinder")
    s"ground coffee of ${beans}"
  }

  def heatWater(water: Water) = water.copy(temperature = 85)
  def heatWater(water: Water, temperature: Int): Future[Water] = Future {
    println("Starting boiler")
    Thread.sleep(Random.nextInt(2000))
    println("Stopped boiler")
    heatWater(water)
  }

  def frothMilk(milk: Milk) = Future {
    println("Starting to froth milk")
    Thread.sleep(Random.nextInt(2000))
    println("Stopped frothing milk")
    s"frothed ${milk.get}"
  }
  def frothMilk2(milk: String): FrothedMilk = Future {
    s"$milk"
  }

  def brew(coffee: GroundCoffee, heatedWater: Water) =
    Some(s"espresso of ${coffee} in water heated to ${heatedWater.temperature}")
  def brew2(coffee: String, heatedWater: Water): Future[Espresso] = Future {
    println("Staring brewer")
    Thread.sleep(Random.nextInt(2000))
    println("Stopped brewer")
    // brew(coffee, heatedWater) // Causing Future[T] returned in another Future[T]
    Some(s"espresso of ${coffee} in water heated to ${heatedWater.temperature}")
  }

  def combine(espressso: Espresso, frothedMilk: FrothedMilk) = Future {
    println("Staring to combine espresso and frothed milk")
    Thread.sleep(Random.nextInt(2000))
    println("Done")
    s"cappuccino made with ${espressso} and ${frothedMilk}"
  }
  def combine2: String = {
    val msg = s"Staring to combine ..."
    println(msg) // This will always be last
    msg
  }

  def getWaterFromFilter = Water(27)

  def isTemperatureOkay: Future[Boolean] = {
    isTemperatureOkay(85)
  }

  def isTemperatureOkay(intendedTemperature: Int): Future[Boolean] = {
    // map into a possible future
    // will start mapping once Future[Water] completed successfully
    // else do nothing
    heatWater(getWaterFromFilter, intendedTemperature).map {
      water =>
        {
          println("This is in the future")
          (80 to 85).contains(water.temperature)
        }
    }
  }

  // flatMap vs comprehension
  def prepareCappuccino = ???
  def prepareCappuccino2: Future[String] = {
    val groundCoffee = grind(Some("Arabica Beans"))
    val heatedWater = heatWater(getWaterFromFilter, 85)
    val frothedMilk = frothMilk(Some("Good Day"))
    for {
      ground <- groundCoffee
      water <- heatedWater
      form <- frothedMilk
      espresso <- brew2(ground, water)
    } yield combine2
  }

  def prepareCappuccinoSequentially: Future[Object] = for {
    ground <- grind(Some("Arabica Beans"))
    water <- heatWater(getWaterFromFilter, 85)
    form <- frothMilk(Some("Skimmed milk"))
    espresso <- brew2(ground, water)
  } yield ground

  def main(args: Array[String]): Unit = {
    prepareCappuccino2
    Thread.sleep(5000) // wait for all other threads
  }
}