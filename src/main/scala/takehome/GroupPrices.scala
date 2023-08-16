package takehome

import scala.collection.MapView

object GroupPrices {

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(
      cabinCode: String,
      rateCode: String,
      price: BigDecimal,
      rateGroup: String
  )

  /*
    Assumptions:
      If a rate group has no prices, there will be no results for that rate group in the returned list.
      Prices with rate codes not in the rates list will not be considered.
   */
  def getBestGroupPrices(
      rates: Seq[Rate],
      prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    val rateGroupMap: Map[String, String] =
      rates.map(r => (r.rateCode, r.rateGroup)).toMap

    // Prices with rate codes not in the rates Seq will not be included
    val pricesWithRateGroups: Seq[BestGroupPrice] = prices
      .map(price =>
        rateGroupMap.get(price.rateCode).map { rateGroup =>
          BestGroupPrice(
            price.cabinCode,
            price.rateCode,
            price.price,
            rateGroup
          )
        }
      )
      .flatten

    val groupedCabinPrices: Map[(String, String), Seq[BestGroupPrice]] =
      pricesWithRateGroups.groupBy(bgp => (bgp.cabinCode, bgp.rateGroup))

    groupedCabinPrices.view
      .mapValues(priceList => priceList.sortBy(_.price).headOption)
      .values
      .flatten
      .toSeq
  }
}
