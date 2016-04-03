package com.example

import scala.util.{ Failure, Success, Try }

trait BeforeBaseTypeSystem[T1] {
  type CoffeeBeans = T1
  type Milk = T1
  type Espresso  = T1
}

trait AfterBaseTypeSystem[T2] {
   type GroundCoffee = T2
   type FrothedMilk = T2
   type Cappuccino = T2
}

trait CommonTypeSystem {
  case class Water(temperature: Int)
  
  case class GrindingFault(msg: String) extends Exception(msg)
  case class FrothingFault(msg: String) extends Exception(msg)
  case class WaterBoilingFault(msg: String) extends Exception(msg)
  case class BrewingFault(msg: String) extends Exception(msg)
}

trait CappuccinoSpec[B, A] 
    extends CommonTypeSystem
      with BeforeBaseTypeSystem[B] 
          with AfterBaseTypeSystem[A] {
  def grind(beans: CoffeeBeans): GroundCoffee
  def heatWater(water: Water): Water
  def frothMilk(milk: Milk): FrothedMilk
  def brew(coffee: GroundCoffee, heatedWater: Water): Espresso
  def combine(espressso: Espresso, frothedMilk: FrothedMilk): Cappuccino
  
  def prepareCappuccino: Try[Cappuccino]
}

trait CappuccinoSomeSpec extends CappuccinoSpec[Some[String], Some[String]]

object Main extends CappuccinoSomeSpec {
  def grind(beans: CoffeeBeans) = Some(s"ground coffee of ${beans.get}")
  def heatWater(water: Water) = water.copy(temperature = 85)
  def frothMilk(milk: Milk) = Some(s"frothed ${milk.get}")
  def brew(coffee: GroundCoffee, heatedWater: Water) = 
    Some(s"espresso of ${coffee.get}")
  def combine(espresso: Espresso, frothedMilk: FrothedMilk) = 
    Some(s"cappuccino made with ${espresso.get} and ${frothedMilk.get}")
  def prepareCappuccino = for {
    ground <- Try(grind(Some("Arabica beans")))
    water <- Try(heatWater(Water(25)))
    espresso <- Try(brew(ground, water))
    foam <- Try(frothMilk(Some("Skimmed milk")))
  } yield combine(espresso, foam)
  
  def main(args: Array[String]): Unit = prepareCappuccino match {
    case Success(Some(s)) => println(s)
    case Failure(f) => println(f)
  } 
}