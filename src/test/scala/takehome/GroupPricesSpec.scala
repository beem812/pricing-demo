package takehome

class GroupPricesSpec extends munit.FunSuite {
  import GroupPrices._

  test("Gives correct answer") {
    val expectedResult = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )

    val inputRates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val inputCabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val result = GroupPrices.getBestGroupPrices(inputRates, inputCabinPrices)

    assertEquals(result.sortBy(_.price), expectedResult.sortBy(_.price))
  }

  test("Handles prices with invalid rate codes") {

    val expectedResult = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )

    val inputRates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val inputCabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00),
      CabinPrice("CB", "invalid", 100.00)
    )

    val result = GroupPrices.getBestGroupPrices(inputRates, inputCabinPrices)

    assertEquals(result.sortBy(_.price), expectedResult.sortBy(_.price))
  }

  test("Only gives prices for rate groups in the rate list") {
    val expectedResult = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CB", "M1", 230.00, "Military")
    )

    val inputRates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military")
    )

    val inputCabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00),
      CabinPrice("CB", "invalid", 100.00)
    )

    val result = GroupPrices.getBestGroupPrices(inputRates, inputCabinPrices)

    assertEquals(result.sortBy(_.price), expectedResult.sortBy(_.price))
  }
}
