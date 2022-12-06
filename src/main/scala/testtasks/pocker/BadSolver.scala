package testtasks.pocker

object BadSolver {
  private val cardRate = Map(
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

  implicit val orderingRanks: Ordering[Char] = Ordering.fromLessThan(cardRate(_) > cardRate(_))
  implicit val orderingMaxRates: Ordering[String] = Ordering.fromLessThan { (str1, str2) =>
    str1.zip(str2).dropWhile { case (rate1, rate2) => rate1 == rate2 }.headOption match {
      case Some((rate1, rate2)) => cardRate(rate1) < cardRate(rate2)
      case None => true
    }
  }

  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => texasHoldemSol(board, hands)
      case "omaha-holdem" :: board :: hands => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => fiveHandDrawSol(hands)
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }

  def texasHoldemSol(board: String, hands: List[String]): String = {
    val allThreeBoardCombinations = getThreeBoardCombinations(board)
    val allFourBoardCombinations = getFourBoardCombinations(board)
    val boardRate = checkAllPossibleCombinations(board)
    val handsWithRate = hands.foldLeft(Map.empty[String, Int]) { (acc, hand) =>
      val (leftCard, rightCard) = hand.splitAt(2)

      acc + (hand ->
        (boardRate ::
          (allThreeBoardCombinations.map(_ + hand) ::: allFourBoardCombinations.map(_ + leftCard) ::: allFourBoardCombinations.map(_ + rightCard))
            .map(checkAllPossibleCombinations))
          .max)
    }

    combineHands(handsWithRate)
  }

  def fiveHandDrawSol(hands: List[String]): String = {
    val handsWithRate = hands.foldLeft(Map.empty[String, Int]) { (acc, combination) =>
      acc + (combination -> checkAllPossibleCombinations(combination))
    }

    combineHands(handsWithRate)
  }

  def combineHands(handsWithRate: Map[String, Int]): String = {
    handsWithRate
      .groupMap(_._2)(_._1)
      .toList
      .view
      .sortBy(_._1)
      .map(_._2.toList)
      .map(sortSameRates)
      .mkString(" ")
  }

  def getThreeBoardCombinations(board: String): List[String] = {
    val cardOfBoard = board.grouped(2).toList

    (for {
      h1 :: t1 <- cardOfBoard.tails
      h2 :: t2 <- t1.tails
      h3 <- t2
    } yield s"$h1$h2$h3").toList
  }

  def getFourBoardCombinations(board: String): List[String] = {
    val cardOfBoard = board.grouped(2).toList

    (for {
      h1 :: t1 <- cardOfBoard.tails
      h2 :: t2 <- t1.tails
      h3 :: t3 <- t2.tails
      h4 <- t3
    } yield s"$h1$h2$h3$h4").toList
  }

  def checkAllPossibleCombinations(combination: String): Int = {
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

  def checkStraightFlush(suits: String, ranks: String): Option[Int] = {
    if (suits.groupBy(identity).size == 1 && ranks.groupBy(identity).size == 5) {
      val rates = ranks.map(cardRate).sorted.toList
      val minRate = rates.min
      val maxRate = rates.max

      if (maxRate - minRate == 4 || rates.collect { case 13 => 0 }.sum == 10) {
        Some(900 + (if (maxRate - minRate == 4) rates.last else 4))
      }
      else
        None
    }
    else
      None
  }

  def checkFourOfAKind(ranks: String): Option[Int] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.exists(_ == 4)) {
      Some(800 + cardRate(groupsOfRanks.filter(pred => pred._2 == 4).keys.head))
    }
    else {
      None
    }
  }

  def checkFullHouse(ranks: String): Option[Int] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.size == 2) {
      Some(700 + cardRate(groupsOfRanks.filter(pred => pred._2 == 3).keys.head))
    }
    else {
      None
    }
  }

  def checkFlush(suits: String, ranks: String): Option[Int] = {
    if (suits.groupBy(identity).size == 1) {
      Some(600 + ranks.map(cardRate).max)
    }
    else {
      None
    }
  }

  def checkStraight(ranks: String): Option[Int] = {
    val rates = ranks.toList.map(cardRate).sorted
    val minRate = rates.min
    val maxRate = rates.max

    if (ranks.groupBy(identity).size == 5 &&
      (maxRate - minRate == 4 || rates.collect { case 13 => 0 }.sum == 10)) {
      Some(500 + (if (maxRate - minRate == 4) rates.last else 4))
    }
    else
      None
  }

  def checkThreeOfAKind(ranks: String): Option[Int] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.exists(_ == 3)) {
      Some(400 + cardRate(groupsOfRanks.filter(pred => pred._2 == 3).keys.head))
    }
    else
      None
  }

  def checkTwoPairs(ranks: String): Option[Int] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.count(_ == 2) == 2) {
      Some(300 + groupsOfRanks.filter(pred => pred._2 == 2).keys.map(cardRate).max)
    }
    else
      None
  }

  def checkPair(ranks: String): Option[Int] = {
    val groupsOfRanks = ranks.groupMapReduce(identity)(_ => 1)(_ + _)

    if (groupsOfRanks.values.exists(_ == 2)) {
      Some(200 + cardRate(groupsOfRanks.filter(pred => pred._2 == 2).keys.head))
    }
    else
      None
  }

  def checkHighCard(ranks: String): Option[Int] = {
    val groupsOfRanks = ranks.groupBy(identity)

    Some(100 + groupsOfRanks.keys.map(cardRate).max)
  }

  def sortSameRates(hands: List[String]): String = {
    hands
      .map { hand =>
        val rates = hand.filterNot(_.isLower).sorted

        (rates, hand)
      }
      .groupMap(_._1)(_._2)
      .toList
      .view
      .sortBy(_._1)
      .map(_._2.mkString("="))
      .mkString(" ")
  }
}
