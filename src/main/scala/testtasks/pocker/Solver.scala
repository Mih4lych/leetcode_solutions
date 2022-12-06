package testtasks.pocker

import scala.collection.SortedMap

object Solver {
  private val rankRate = Map(
    '2' -> 1,
    '3' -> 2,
    '4' -> 3,
    '5' -> 4,
    '6' -> 5,
    '7' -> 6,
    '8' -> 7,
    '9' -> 8,
    'T' -> 9,
    'J' -> 10,
    'Q' -> 11,
    'K' -> 12,
    'A' -> 13,
  )

  //Ordering cards ranks in the combination
  private implicit val orderingRanks: Ordering[Char] = Ordering.fromLessThan(rankRate(_) > rankRate(_))
  //Ordering cards by their ranks
  private implicit val orderingCombinationRates: Ordering[String] = Ordering.fromLessThan{ (str1, str2) =>
    str1.zip(str2).dropWhile{case (rate1, rate2) => rate1 == rate2}.headOption match {
      case Some((rate1, rate2)) => rankRate(rate1) < rankRate(rate2)
      case None => true
    }
  }

  /**
   * Process all solutions
   *
   * @param line - line of all cards (format: game-name (board) hands)
   * @return If the line has a proper game name, method returns sorted hands. If not, method returns an error text.
   */
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => texasHoldemSolution(board, hands)
      case "omaha-holdem" :: board :: hands => omahaHoldemSolution(board, hands)
      case "five-card-draw" :: hands => fiveHandDrawSolution(hands)
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }

  /**
   * Compute the result for Five Card Draw game.
   *
   * @param hands - list of hands
   * @return Sorted hands
   */
  def fiveHandDrawSolution(hands: List[String]): String = {
    getSolution(hands) { hand =>
      getCombinationData(hand)
    }
  }

  /**
   * Compute the result for Texas Hold'em game.
   *
   * @param hands - list of hands
   * @return Sorted hands
   */
  def texasHoldemSolution(board: String, hands: List[String]): String = {
    getSolution(hands) { hand =>
      //combine all possible combinations
      (board ::
        (getThreeCardsCombinations(board).map(_ + hand) :::
          getFourCardsCombinations(board).flatMap(fourCards => getOneCardCombinations(hand).map(_ + fourCards))))
        .map(getCombinationData)
        .max
    }
  }

  /**
   * Compute the result for Omaha Hold'em game.
   *
   * @param hands - list of hands
   * @return Sorted hands
   */
  def omahaHoldemSolution(board: String, hands: List[String]): String = {
    getSolution(hands) { hand =>
      //combine all possible combinations
      (board ::
        (getOneCardCombinations(board).map(_ + hand) :::
          getTwoCardsCombinations(board).flatMap(twoBoardCards => getThreeCardsCombinations(hand).map(_ + twoBoardCards)) :::
          getThreeCardsCombinations(board).flatMap(threeBoardCards => getTwoCardsCombinations(hand).map(_ + threeBoardCards)) :::
          getFourCardsCombinations(board).flatMap(fourBoardCards => getOneCardCombinations(hand).map(_ + fourBoardCards))))
        .map(getCombinationData)
        .max
    }
  }

  /**
   * Compute the result for all games.
   *
   * @param hands - list of hands
   * @param combinationDataFunc - function for calculating a max possible result of hand combinations
   *                              (returning a tuple of (rating of the combination, string for the further sorting))
   * @return Sorted hands
   */
  private def getSolution(hands: List[String])(combinationDataFunc: String => (Int, String)): String = {
    val handsWithRate = hands.foldLeft(SortedMap.empty[Int, List[(String, String)]]) { (acc, hand) =>
      val maxCombinationData = combinationDataFunc(hand)

      acc + (maxCombinationData._1 -> ((maxCombinationData._2, hand) :: acc.getOrElse(maxCombinationData._1, Nil)))
    }

    combineHands(handsWithRate)
  }

  /**
   * Split a group of cards one by one
   *
   * @param groupOfCards - cards for breaking
   * @return - list of split cards
   */
  private def getOneCardCombinations(groupOfCards: String): List[String] = {
    groupOfCards.grouped(2).toList
  }

  /**
   * Split a group of cards into groups of two cards
   *
   * @param groupOfCards - cards for breaking
   * @return - list of split cards
   */
  private def getTwoCardsCombinations(groupOfCards: String): List[String] = {
    val listOfCards = groupOfCards.grouped(2).toList

    (for {
      h1 :: t1 <- listOfCards.tails
      h2 <- t1
    } yield s"$h1$h2").toList
  }

  /**
   * Split a group of cards into groups of three cards
   *
   * @param groupOfCards - cards for breaking
   * @return - list of split cards
   */
  private def getThreeCardsCombinations(groupOfCards: String): List[String] = {
    val listOfCards = groupOfCards.grouped(2).toList

    (for {
      h1 :: t1 <- listOfCards.tails
      h2 :: t2 <- t1.tails
      h3 <- t2
    } yield s"$h1$h2$h3").toList
  }

  /**
   * Split a group of cards into groups of four cards
   *
   * @param groupOfCards - cards for breaking
   * @return - list of split cards
   */
  private def getFourCardsCombinations(groupOfCards: String): List[String] = {
    val listOfCards = groupOfCards.grouped(2).toList

    (for {
      h1 :: t1 <- listOfCards.tails
      h2 :: t2 <- t1.tails
      h3 :: t3 <- t2.tails
      h4 <- t3
    } yield s"$h1$h2$h3$h4").toList
  }

  /**
   * Get a combination's data
   *
   * @param combination - checkable combination
   * @return tuple of (rating of the combination, string for the further sorting)
   */
  private def getCombinationData(combination: String): (Int, String) = {
    val (suits, ranks) = combination.partition(_.isLower)

    checkStraightFlush(suits, ranks)
      .getOrElse(checkFourOfAKind(ranks)
        .getOrElse(checkFullHouse(ranks)
          .getOrElse(checkFlush(suits, ranks)
            .getOrElse(checkStraight(ranks)
              .getOrElse(checkThreeOfAKind(ranks)
                .getOrElse(checkTwoPairs(ranks)
                  .getOrElse(checkPair(ranks)
                    .getOrElse(checkHighCard(ranks).get))))))))
  }

  /**
   * Check Straight Flush
   *
   * @param suits - all suits of the combination
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Straight Flush or None if not.
   */
  private def checkStraightFlush(suits: String, ranks: String): Option[(Int, String)] = {
    if (suits.groupBy(identity).size == 1 && ranks.groupBy(identity).size == 5) {
      val sortedRanks = ranks.sorted
      val rates = sortedRanks.map(rankRate).toList
      val minRate = rates.min
      val maxRate = rates.max

      if (maxRate - minRate == 4 || rates.collect{case 13 => 0}.sum == 10) {
        Some((9, if (maxRate - minRate == 4) sortedRanks.reverse.head.toString else "5"))
      }
      else
        None
    }
    else
      None
  }

  /**
   * Check Four of a kind
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Four of a kind or None if not.
   */
  private def checkFourOfAKind(ranks: String): Option[(Int, String)] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.exists(_ == 4)) {
      Some((8, groupsOfRanks.filter(pred => pred._2 == 4).keys.head.toString))
    }
    else {
      None
    }
  }

  /**
   * Check Full House
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Full House or None if not.
   */
  private def checkFullHouse(ranks: String): Option[(Int, String)] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.size == 2) {
      Some(7, groupsOfRanks.filter(pred => pred._2 == 3).keys.head.toString)
    }
    else {
      None
    }
  }

  /**
   * Check Flush
   *
   * @param suits - all suits of the combination
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Flush or None if not.
   */
  private def checkFlush(suits: String, ranks: String): Option[(Int, String)] = {
    if (suits.groupBy(identity).size == 1) {
      Some(6, ranks.sorted)
    }
    else {
      None
    }
  }

  /**
   * Check Straight
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Straight or None if not.
   */
  private def checkStraight(ranks: String): Option[(Int, String)] = {
    val sortedRanks = ranks.sorted
    val rates = sortedRanks.map(rankRate).toList
    val minRate = rates.min
    val maxRate = rates.max

    if (ranks.groupBy(identity).size == 5 &&
      (maxRate - minRate == 4 || rates.collect{case 13 => 0}.sum == 10)) {
      Some((5, if (maxRate - minRate == 4) sortedRanks.reverse.head.toString else "5"))
    }
    else
      None
  }

  /**
   * Check Three of a kind
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Three of a kind or None if not.
   */
  private def checkThreeOfAKind(ranks: String): Option[(Int, String)] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.exists(_ == 3)) {
      Some((4, groupsOfRanks.filter(pred => pred._2 == 3).keys.head.toString))
    }
    else
      None
  }

  /**
   * Check Two pairs
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Two pairs or None if not.
   */
  private def checkTwoPairs(ranks: String): Option[(Int, String)] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.count(_ == 2) == 2) {
      Some((3, groupsOfRanks.filter(_._2 == 2).keys.mkString.sorted + groupsOfRanks.filterNot(_._2 == 2).keys.mkString))
    }
    else
      None
  }

  /**
   * Check Pair
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's Pair or None if not.
   */
  private def checkPair(ranks: String): Option[(Int, String)] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.exists(_ == 2)) {
      Some(2, groupsOfRanks.filter(_._2 == 2).keys.mkString.sorted + groupsOfRanks.filterNot(_._2 == 2).keys.mkString.sorted)
    }
    else
      None
  }


  /**
   * Check High card
   *
   * @param ranks - all ranks of the combination
   * @return Some(tuple of (rating of the combination, string for the further sorting)) if it's High card.
   */
  private def checkHighCard(ranks: String): Option[(Int, String)] = {
    Some(1, ranks.sorted)
  }

  /**
   * Combine all results
   *
   * @param combinationsData - map with the next information - (rating of the combination, (string for the sorting, hand))
   * @return - Sorted hands
   */
  private def combineHands(combinationsData: SortedMap[Int, List[(String, String)]]): String = {
    combinationsData
      .view
      .mapValues(sortSameCombinations)
      .values
      .mkString(" ")
  }


  /**
   * Combine all results of the same combination rating
   *
   * @param handsWithSortingData - list with the next information - (string for the sorting, hand)
   * @return - Sorted hands inside the same combination
   */
  private def sortSameCombinations(handsWithSortingData: List[(String, String)]): String = {
    handsWithSortingData
      .groupMap(_._1)(_._2)
      .toList
      .view
      .sortBy(_._1)
      .map(_._2.mkString("="))
      .mkString(" ")
  }
}
