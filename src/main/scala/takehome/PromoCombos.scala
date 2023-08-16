package takehome

object PromoCombos {
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    val combosWithSubCombos = allPromotions.zipWithIndex.map { case (p, i) =>
      combinablePromotions(
        p.code,
        allPromotions.drop(i) // Only include promotion options that haven't already been used to start combos
      )
    }.flatten

    // Remove entries that are slices of bigger combos. e.g. remove Seq(P4, P5) if Seq(P1, P4, P5) exists
    combosWithSubCombos.filterNot(isSubCombo(_, combosWithSubCombos))
  }

  def combinablePromotions(promotion: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allPromotions
      .find(_.code == promotion)
      .map { promo =>
        buildMaxCombos(promo, allPromotions)
      }
      .getOrElse(Seq.empty[PromotionCombo])
  }

  // Build all max size combos for given promotion
  def buildMaxCombos(
      promotion: Promotion,
      remainingPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    if (remainingPromotions.length == 0) {
      return Nil
    }

    val newCombo = buildCombo(
      Set(promotion.code),
      promotion.notCombinableWith.toSet,
      remainingPromotions
    )

    newCombo.map(s => PromotionCombo(s.toSeq)).toSeq
  }

  // Non-tco recursive function that should create a max stack depth of N where N is the total number of promos passed to the remainingPromotions argument
  def buildCombo(
      startingCombo: Set[String],
      accumulatedBanList: Set[String],
      remainingPromotions: Seq[Promotion]
  ): Set[Set[String]] = {
    remainingPromotions.headOption.fold(Set(startingCombo)) { currentPromo =>
      if (incompatible(currentPromo, startingCombo, accumulatedBanList)) {
        buildCombo(
          startingCombo,
          accumulatedBanList,
          remainingPromotions.tail
        )
      } else {
        // promo code combos that include currentPromo
        val useCurrent = buildCombo(
          startingCombo + currentPromo.code,
          accumulatedBanList ++ currentPromo.notCombinableWith,
          remainingPromotions.tail
        )

        // promo code combos that skip currentPromo
        val skipCurrent = buildCombo(startingCombo, accumulatedBanList, remainingPromotions.tail)

        val combined = useCurrent ++ skipCurrent
        combined.filter(combo => !combined.exists(oCombo => combo.subsetOf(oCombo) && !(oCombo == combo)))
      }
    }
  }

  def incompatible(
      promotion: Promotion,
      currentPromoCodes: Set[String],
      banList: Set[String]
  ) =
    banList.contains(promotion.code) || currentPromoCodes.exists(code => promotion.notCombinableWith.contains(code))

  def isSubCombo(combo: PromotionCombo, allCombos: Seq[PromotionCombo]) =
    allCombos.exists(comboToCheckAgainst =>
      comboToCheckAgainst.promotionCodes.containsSlice(combo.promotionCodes) && !(comboToCheckAgainst == combo)
    )
}
