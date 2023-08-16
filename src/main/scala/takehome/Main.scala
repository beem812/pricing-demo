package takehome
import GroupPrices.{Rate, CabinPrice}
import PromoCombos.{Promotion}

object Main {
  val groupPriceRates = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val groupPriceCabinPrices = List(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  def demoGroupPrices = GroupPrices.getBestGroupPrices(
    groupPriceRates,
    groupPriceCabinPrices
  )

  val combinablePromotions = List(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
  )

  def demoAllPromotionCombos = PromoCombos.allCombinablePromotions(combinablePromotions)

  def demoCombosForPromo(promoCode: String) = PromoCombos.combinablePromotions(promoCode, combinablePromotions)

  def main(args: Array[String]) = {
    println()
    println()
    args.toList match {
      case "-gp" :: Nil =>
        println(s"The best group prices are: $demoGroupPrices")
      case "-ac" :: Nil =>
        println(s"The list of all max promotion combos is: $demoAllPromotionCombos")
      case "-pc" :: promo :: Nil =>
        println(s"The list of promotion combos including the promotion $promo are: ${demoCombosForPromo(promo)}")
      case _ => println("Invalid arguments, possible arguments are '-gp', '-ac', '-pc <promo code>'")
    }
    println()
    println()
  }
}
