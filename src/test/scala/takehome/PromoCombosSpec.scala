package takehome

class PromoCombosSpec extends munit.FunSuite {
  import PromoCombos._

  test("allCombinablePromotions() - gives known good result") {
    val input = List(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )
    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    val result = allCombinablePromotions(input)
    assertEquals(
      result.sortBy(_.promotionCodes.sorted.mkString),
      expected.sortBy(_.promotionCodes.sorted.mkString)
    )
  }

  test("allCombinablePromotions() - largest group is not made from taking all eligible promotions in order") {
    val input = List(
      Promotion("P1", Seq("P5")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P6", "P7")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2", "P1")),
      Promotion("P6", Seq("P3")),
      Promotion("P7", Seq("P3"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2", "P3")),
      PromotionCombo(Seq("P1", "P2", "P6", "P7")),
      PromotionCombo(Seq("P1", "P3", "P4")),
      PromotionCombo(Seq("P1", "P4", "P6", "P7")),
      PromotionCombo(Seq("P3", "P4", "P5")),
      PromotionCombo(Seq("P4", "P5", "P6", "P7"))
    )

    val result = allCombinablePromotions(input)

    println(result)
    assertEquals(result, expected)
  }

  test("buildMaxCombos() - largest group is not made from taking all eligible promotions in order") {
    val input = List(
      Promotion("P1", Seq("P5")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P6", "P7")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2", "P1")),
      Promotion("P6", Seq("P3")),
      Promotion("P7", Seq("P3"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2", "P3")),
      PromotionCombo(Seq("P1", "P2", "P6", "P7")),
      PromotionCombo(Seq("P1", "P3", "P4")),
      PromotionCombo(Seq("P1", "P4", "P6", "P7"))
    )

    val result = buildMaxCombos(input.head, input.tail)

    println(result)
    assertEquals(result, expected)
  }
}
