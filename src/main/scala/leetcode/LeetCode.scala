package leetcode

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint}
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

object LeetCode extends App {
  def containsNearbyDuplicate(nums: Array[Int], k: Int): Boolean = {
    val numLength = nums.length

    (0 until (numLength - 1))
      .exists(i =>
        ((i + 1) to i + k)
          .exists(j => j < numLength && nums(i) == nums(j) && Math.abs(i - j) <= k)
      )
  }

  def containsWithoutSeq(nums: Array[Int], k: Int): Boolean = {
    val cache = mutable.HashSet[Int]()

    nums.zipWithIndex.exists { pair =>
      if (pair._2 > k) cache.remove(nums(pair._2 - k - 1))
      !cache.add(pair._1)
    }
  }

  def reverse(x: Int): Int = {
    if (x == 0) 0
    else Try(Math.abs(x).toString.reverse.dropWhile(_ == '0').toInt * x.sign).getOrElse(0)
  }

  def myAtoi(s: String): Int = {
    val trimmedStr = s.trim
    Try {
      val bigInt = {
        if (trimmedStr.startsWith("-") || trimmedStr.startsWith("+")) {
          BigInt(s"${trimmedStr.head}${trimmedStr.drop(1).takeWhile(_.isDigit)}")
        }
        else {
          BigInt(trimmedStr.takeWhile(_.isDigit))
        }
      }

      if (bigInt.isValidInt) {
        bigInt.toInt
      }
      else {
        if (trimmedStr.startsWith("-")) Integer.MIN_VALUE else Integer.MAX_VALUE
      }
    }.getOrElse(0)
  }

  def threeSumForBigMemory(nums: Array[Int]): List[List[Int]] = {
    nums.sorted.span(_ < 0) match {
      case (_, Array()) => Nil
      case (negNums, posNums) =>
        negNums.toList.combinations(2).toSet.withFilter(pair => posNums.contains(-pair.sum)).map(pair => -pair.sum :: pair).toList :::
          posNums.toList.combinations(2).toSet.withFilter(pair => negNums.contains(-pair.sum) || (pair.sum == 0 && posNums.startsWith(Array(0, 0, 0)))).map(pair => -pair.sum :: pair).toList
    }
  }

  def threeSumFirst(nums: Array[Int]): List[List[Int]] = {
    val numLength = nums.length
    val numsWithIndex = nums.zipWithIndex.toMap
    val setOfTwoSum = mutable.HashSet[(Int, Int)]()

    (for {
      i <- 0 to numLength - 2
      j <- i + 1 to numLength - 1
      twoSum = nums(i) + nums(j) if setOfTwoSum.add(nums(i).min(nums(j)), nums(i).max(nums(j))) && numsWithIndex.contains(-twoSum) && numsWithIndex(-twoSum) != i && numsWithIndex(-twoSum) != j
    } yield List(nums(i), nums(j), -twoSum)).toList
  }

  def threeSumAnotherTry(nums: Array[Int]): List[List[Int]] = {
    val numLength = nums.length
    val sortedNums = nums.sorted
    val listBuffer: ListBuffer[List[Int]] = ListBuffer.empty

    def solWithPointers(left: Int = 1, right: Int = numLength - 1, c: Int = 0): List[List[Int]] = {
      if (right <= left) {
        LazyList.range(c + 1, numLength).find(newC => sortedNums(c) != sortedNums(newC)) match {
          case Some(n) => solWithPointers(left = n + 1, c = n)
          case _ => listBuffer.toList
        }
      }
      else if (c >= numLength - 3) listBuffer.toList
      else {
        val neededNum = -sortedNums(c)
        sortedNums(left) + sortedNums(right) match {
          case x if x < neededNum => solWithPointers(left + 1, right, c)
          case x if x > neededNum => solWithPointers(left, right - 1, c)
          case _ =>
            listBuffer += List(sortedNums(left), sortedNums(right), -neededNum)
            LazyList.range(right - 1, left, -1).find(newRight => sortedNums(right) != sortedNums(newRight)) match {
              case Some(n) => solWithPointers(left, n, c)
              case _ => solWithPointers(left, left, c)
            }
        }
      }
    }

    solWithPointers()
  }

  def checkSubarraySumFirst(nums: Array[Int], k: Int): Boolean = {
    def recSolution(ind: Int = 0, curSum: Int = 0, mapOfRems: Map[Int, Int] = Map.empty): Boolean = {
      if (ind == nums.length) false
      else {
        val newSum = curSum + nums(ind)
        val curRem = newSum % k
        if (curSum == 0) true
        else if (mapOfRems.contains(curRem) && mapOfRems(curRem) < ind) true
        else recSolution(ind + 1, newSum, mapOfRems + (curRem -> ind))
      }
    }

    recSolution()
  }

  def groupAnagramsFirst(strs: Array[String]): List[List[String]] = {
    import scala.collection.mutable
    import scala.collection.mutable.ListBuffer

    strs.foldLeft(mutable.HashMap[String, ListBuffer[String]]()) { (map, string) =>
      map.getOrElseUpdate(string.sorted, ListBuffer[String]()) += string

      map
    }.values.view.map(_.toList).toList
  }

  def groupAnagramsNew(strs: Array[String]): List[List[String]] = {
    strs.groupBy(_.sorted).values.view.map(_.toList).toList
  }

  def findBall(grid: Array[Array[Int]]): Array[Int] = {
    val rows = grid.length
    val columns = grid(0).length

    def findBallRec(curRowIndex: Int, curColumnIndex: Int): Int = {
      if (curRowIndex == rows) curColumnIndex
      else {
        val curRow = grid(curRowIndex)
        val direction = curRow(curColumnIndex)

        if (curColumnIndex + direction < 0 || curColumnIndex + direction == columns) -1
        else {

          if (direction != curRow(curColumnIndex + direction)) -1
          else findBallRec(curRowIndex + 1, curColumnIndex + curRow(curColumnIndex))
        }
      }
    }

    grid(0).indices.map(columnIndex => findBallRec(0, columnIndex)).toArray
  }

  def letterCombinations(digits: String): List[String] = {
    val mapOfLetters = Map(2 -> List("a", "b", "c"),
      3 -> List("d", "e", "f"),
      4 -> List("g", "h", "i"),
      5 -> List("j", "k", "l"),
      6 -> List("m", "n", "o"),
      7 -> List("p", "q", "r", "s"),
      8 -> List("t", "u", "v"),
      9 -> List("w", "x", "w", "z"))

    digits.toSeq.view.map(_.asDigit).foldLeft(List.empty[String]) { (acc, str) =>
      acc match {
        case Nil => mapOfLetters(str)
        case _ =>
          for {
            accElem <- acc
            strElem <- mapOfLetters(str)
          } yield accElem + strElem
      }
    }
  }

  def longestPalindromeFirst(words: Array[String]): Int = {
    val wordsLength = words.length
    val mapOfPotentialPalindrome: mutable.Map[String, Int] = mutable.Map.empty

    def longestPalindromeRec(currentPalinLength: Int = 0, currentIndex: Int = 0): Int = {
      if (currentIndex == wordsLength) currentPalinLength + (if (mapOfPotentialPalindrome.keys.exists(_.distinct.length == 1)) 2 else 0)
      else {
        val curRevWord = words(currentIndex).reverse

        if (mapOfPotentialPalindrome.contains(curRevWord)) {
          mapOfPotentialPalindrome(curRevWord) -= 1
          if (mapOfPotentialPalindrome(curRevWord) == 0) {
            mapOfPotentialPalindrome.remove(curRevWord)
          }

          longestPalindromeRec(currentPalinLength + 4, currentIndex + 1)
        }
        else {
          mapOfPotentialPalindrome += (words(currentIndex) -> (mapOfPotentialPalindrome.getOrElse(words(currentIndex), 0) + 1))

          longestPalindromeRec(currentPalinLength, currentIndex + 1)
        }
      }
    }

    longestPalindromeRec()
  }

  def reverseVowels(s: String): String = {
    val vowels = Set('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')

    def reverseVowelsRec(left: Int = 0, right: Int = s.length - 1, curVowelToSwap: Option[Char] = None, curString: String = s): String = {
      if (left >= right) curString
      else {
        curVowelToSwap match {
          case Some(char) =>
            if (vowels(curString(right))) {
              reverseVowelsRec(left + 1, right - 1, None, curString.patch(left, curString(right).toString, 1).patch(right, char.toString, 1))
            }
            else {
              reverseVowelsRec(left, right - 1, curVowelToSwap, curString)
            }
          case _ if vowels(curString(left)) =>
            reverseVowelsRec(left, right, Some(curString(left)), curString)
          case _ =>
            reverseVowelsRec(left + 1, right, None, curString)
        }
      }
    }

    reverseVowelsRec()
  }

  def maximum69Number (num: Int): Int = {
    num.toString.zipWithIndex.find(_._1 == '6') match {
      case Some((_, index)) =>num.toString.patch(index, "9", 1).toInt
      case _ => num
    }
  }

  def makeGood(s: String): String = {
    def makeGoodRec(curString: String = s): String = {
      curString.zipWithIndex.find(pair => pair._2 < curString.length && curString(pair._2) != curString(pair._2 + 1) && curString(pair._2).toLong == curString(pair._2 + 1).toLower) match {
        case Some(pair) => makeGoodRec(curString.patch(pair._2, "", 2))
        case _ => curString
      }
    }

    makeGoodRec()
  }

  def makeGoodNext(s: String): String = {
    s.foldLeft("") {(acc, char) =>
      if (acc.headOption.nonEmpty && acc.head != char && acc.head.toLower.equals(char.toLower)) acc.tail
      else char + acc
    }.reverse
  }

  def minWindow(s: String, t: String): String = {
    val lengthOfS = s.length
    val lengthOfT = t.length
    val seqOfT = t.toSeq
    val mapWithCount = seqOfT.groupMapReduce(identity)(_ => 1)(_ + _)
    val curSetOfT = mutable.HashSet(seqOfT: _*)
    val curMapWithCountOfT = mutable.HashMap(mapWithCount.keys.map(k => k -> 0).toSeq: _*)

    def recCheck(curLeftInd: Int = 0,
                 curRightInd: Int = 0,
                 curWindow: Option[(Int, Int)] = None): Option[(Int, Int)] = {
      if (curSetOfT.nonEmpty) {
        if (curRightInd >= lengthOfS) {
          curWindow
        }
        else {
          val curChar = s(curRightInd)
          if (curMapWithCountOfT.contains(curChar)) {
            curMapWithCountOfT(curChar) += 1

            if (curSetOfT(curChar) && curMapWithCountOfT(curChar) == mapWithCount(curChar)) {
              curSetOfT.remove(curChar)
            }
          }
          if (curSetOfT.nonEmpty) {
            recCheck(curLeftInd, curRightInd + 1, curWindow)
          }
          else {
            recCheck(curLeftInd, curRightInd, getMinResult(curWindow, Some((curLeftInd, curRightInd))))
          }
        }
      }
      else {
        if (curLeftInd >= lengthOfS - lengthOfT + 1) {
          curWindow
        }
        else {
          val curChar = s(curLeftInd)

          if (curMapWithCountOfT.contains(curChar)) {
            curMapWithCountOfT(curChar) -= 1

            if (!curSetOfT(curChar) && curMapWithCountOfT(curChar) < mapWithCount(curChar)) {
              curSetOfT.add(curChar)
            }
          }

          if (curSetOfT.isEmpty) {
            recCheck(curLeftInd + 1, curRightInd, getMinResult(curWindow, Some((curLeftInd + 1, curRightInd))))
          }
          else {
            recCheck(curLeftInd + 1, curRightInd + 1, curWindow)
          }
        }
      }
    }

    def getMinResult(curResult: Option[(Int, Int)], newResult: Option[(Int, Int)]): Option[(Int, Int)] = {
      (curResult, newResult) match {
        case (None, n) => n
        case (c, None) => c
        case (Some((cl, cr)), Some((nl, nr))) =>
          if (cr - cl < nr - nl) curResult else newResult
      }
    }

    recCheck() match {
      case Some((i, j)) => s.substring(i, j + 1)
      case None => ""
    }
  }

  def maxArea(height: Array[Int]): Int = {
    val arrayLength = height.length

    def findMaxArea(leftIndex: Int = 0, rightIndex: Int = arrayLength - 1, maxAmountOfWater: Int = 0): Int = {
      if (leftIndex == rightIndex) {
        maxAmountOfWater
      } else {
        val curMax = (height(leftIndex).min(height(rightIndex)) * (rightIndex - leftIndex)).max(maxAmountOfWater)

        if (height(leftIndex) < height(rightIndex)) {
          findMaxArea(leftIndex + 1, rightIndex, curMax)
        }
        else {
          findMaxArea(leftIndex, rightIndex - 1, curMax)
        }
      }
    }

    findMaxArea()
  }

  def findErrorNums(nums: Array[Int]): Array[Int] = {
    Array(nums.diff(nums.distinct).distinct.head, (1 to nums.length).toArray.diff(nums).head)

    // (1, 1, 3, 4, 5, 6)
    // (1, 2, 3, 5, 5, 6)
  }

  def romanToInt(s: String): Int = {
    def getIntByRoman(romanNum: Char): Int = {
      romanNum match {
        case 'I' => 1
        case 'V' => 5
        case 'X' => 10
        case 'L' => 50
        case 'C' => 100
        case 'D' => 500
        case 'M' => 1000
        case _ => 0
      }
    }

    def romandToIntRec(listRem: List[Int] = s.toSeq.map(getIntByRoman).toList, result: Int = 0): Int = {
      listRem match {
        case firstNum::secondNum::rem if firstNum >= secondNum => romandToIntRec(secondNum::rem, result + firstNum)
        case firstNum::secondNum::rem if firstNum < secondNum => romandToIntRec(rem, secondNum - firstNum + result)
        case lastNum::Nil => result + lastNum
        case _ => result
      }
    }
    romandToIntRec()
  }


  def checkSubarraySum(nums: Array[Int], k: Int): Boolean = {
    val numsLength = nums.length
    def recSolution(startIndex: Int = 0, curIndex: Int = 1, curSum: Int = 0): Boolean = {
      if (startIndex == numsLength - 1) false
      else if (curIndex == numsLength) recSolution(startIndex + 1, startIndex + 2)
      else {
        val newSum = curSum + nums(curIndex)
        if (!(newSum % k == 0)) recSolution(startIndex, curIndex = curIndex + 1, curSum = newSum)
        else true
      }
    }

    recSolution()
  }

  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    val length = strs.length
    strs.zipWithIndex.foldLeft((ListBuffer.empty[List[String]], mutable.HashSet[String]())) { (data, strWithInd) =>
      if (!data._2(strWithInd._1)) {
        val listBuf = ListBuffer(strWithInd._1)

        ((strWithInd._2 + 1) until length)
          .withFilter(indForCheck => strs(indForCheck).isEmpty && strWithInd._1.isEmpty || strs(indForCheck) != strWithInd._1 && strs(indForCheck).toSeq.diff(strWithInd._1).isEmpty)
          .foreach { indForWrite =>
            data._2 += strs(indForWrite)
            listBuf += strs(indForWrite)
          }
        data._1 += listBuf.toList
      }
      data
    }._1.toList
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    import scala.collection.mutable.ListBuffer

    val numLength = nums.length
    val sortedNums = nums.sorted
    val listBuffer: ListBuffer[List[Int]] = ListBuffer.empty

    def solWithPointers(left: Int = 1, right: Int = numLength - 1, c: Int = 0): List[List[Int]] = {
      if (right <= left) {
        LazyList.range(c + 1, numLength).find(newC => sortedNums(c) != sortedNums(newC)) match {
          case Some(n) => solWithPointers(left = n + 1, c = n)
          case _ => listBuffer.toList
        }
      }
      else if (c >= numLength - 2) listBuffer.toList
      else {
        val neededNum = -sortedNums(c)
        sortedNums(left) + sortedNums(right) match {
          case x if x < neededNum => solWithPointers(left + 1, right, c)
          case x if x > neededNum => solWithPointers(left, right - 1, c)
          case _ =>
            listBuffer += List(sortedNums(left), sortedNums(right), -neededNum)
            LazyList.range(right - 1, left, -1).find(newRight => sortedNums(right) != sortedNums(newRight)) match {
              case Some(n) => solWithPointers(left, n, c)
              case _ => solWithPointers(left, left, c)
            }
        }
      }
    }

    solWithPointers()
  }

  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    val numLength = nums.length
    val sortedNums = nums.sorted

    @tailrec
    def solWithPointers(left: Int = 1, right: Int = numLength - 1, c: Int = 0, closestResult: Option[Int] = None): Option[Int] = {
      if (right <= left) {
        LazyList.range(c + 1, numLength).find(newC => sortedNums(c) != sortedNums(newC)) match {
          case Some(n) => solWithPointers(left = n + 1, c = n, closestResult = closestResult)
          case _ => closestResult
        }
      }
      else if (c >= numLength - 2) closestResult
      else {
        val sum = sortedNums(c) + sortedNums(left) + sortedNums(right)
         val curResult = closestResult match {
           case None => Some(sum)
           case Some(result) =>
             Some{
               if (Math.abs(target - sum) < Math.abs(target - result)) {
                 sum
               }
               else {
                 result
               }
             }
         }

         if (sum <= target) {
           LazyList.range(left + 1, right).find(newLeft => sortedNums(left) != sortedNums(newLeft)) match {
             case Some(n) => solWithPointers(n, right, c, curResult)
             case _ => solWithPointers(right, right, c, curResult)
           }
         }
         else {
           LazyList.range(right - 1, left, -1).find(newRight => sortedNums(right) != sortedNums(newRight)) match {
             case Some(n) => solWithPointers(left, n, c, curResult)
             case _ => solWithPointers(left, left, c, curResult)
           }
         }
      }
    }

    solWithPointers().get
  }

  def minMutation(start: String, end: String, bank: Array[String]): Int = {
    val bankSet = bank.toSet

    def minMutationRec(queue: mutable.Queue[(String, Int)], visited: mutable.HashSet[String] = mutable.HashSet.empty): Int = {
      if (queue.isEmpty) -1
      else {
        val (strToCheck, level) = queue.dequeue()
        if (strToCheck.equals(end)) level
        else {
          bankSet.withFilter(str => str.diff(strToCheck).length == 1 && !visited(str)).foreach{ str =>
            queue.enqueue((str, level + 1))
            visited.add(strToCheck)
          }
          visited.add(strToCheck)

          minMutationRec(queue, visited)
        }
      }
    }

    if (!bankSet(end)) {
      -1
    }
    else {
      val queue = mutable.Queue[(String, Int)]((start, 0))

      minMutationRec(queue)
    }
  }

  def longestPalindrome(words: Array[String]): Int = {
    val wordsLength = words.length
    val setOfPotentialPalindrome: mutable.Set[String] = mutable.Set.empty

    def longestPalindromeRec(currentPalinLength: Int = 0, currentIndex: Int = 0): Int = {
      if (currentIndex == wordsLength) currentPalinLength + (if (setOfPotentialPalindrome.exists(_.distinct.length == 1)) 2 else 0)
      else {
        val curRevWord = words(currentIndex).reverse

        if (setOfPotentialPalindrome(curRevWord)) {
          setOfPotentialPalindrome.remove(curRevWord)
          longestPalindromeRec(currentPalinLength + 4, currentIndex + 1)
        }
        else {
          setOfPotentialPalindrome.add(words(currentIndex))

          longestPalindromeRec(currentPalinLength, currentIndex + 1)
        }
      }
    }

    longestPalindromeRec()
  }

  class StockSpanner() {
    val listOfPrices = ListBuffer.empty[Int]

    def next(price: Int): Int = {
      val result = listOfPrices.takeWhile(_ <= price).length

      listOfPrices.prepend(price)

      result
    }
  }

  def removeDuplicates(s: String): String = {
    s.foldLeft(List.empty[Char]) { (acc, charForCheck) =>
      acc.headOption match {
        case Some(char) =>
          if (char.equals(charForCheck)) acc.tail
          else charForCheck :: acc
        case _ =>
          charForCheck :: acc
      }
    }.reverse.mkString
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    nums.drop(1).foldLeft(1){ (uniq, nextNum) =>
      if (nums(uniq - 1) == nextNum) {
        uniq
      }
      else {
        nums(uniq) = nextNum
        uniq + 1
      }
    }
  }

  class MedianFinder() {
    private val minPrio = mutable.PriorityQueue[Int]()(Ordering.by(x => x))
    private val maxPrio = mutable.PriorityQueue[Int]()(Ordering.by(x => -x))
    def addNum(num: Int): Unit = {
      if (num < minPrio.headOption.getOrElse(Int.MaxValue)) {
        minPrio.enqueue(num)
      }
      else {
        maxPrio.enqueue(num)
      }
      minPrio.size - maxPrio.size match {
        case 2 => maxPrio.enqueue(minPrio.dequeue())
        case -2 => minPrio.enqueue(maxPrio.dequeue())
        case _ =>
      }
      }

      def findMedian(): Double = {
        val minSize = minPrio.size
        val maxSize = maxPrio.size

        if (minSize == maxSize) (minPrio.head + maxPrio.head).toDouble / 2
        else if (minSize > maxSize) minPrio.head
        else maxPrio.head
      }
  }
  //[[6],[],[10],[],[2],[],[6],[],[5],[],[0],[],[6],[],[3],[],[1],[],[0],[],[0],[]]

  def reverseWords(s: String): String = {
    s.trim.split(" ").filter(_.nonEmpty).reverse.mkString(" ")
  }
  def reverseWords_v2(s: String): String = {
    s.trim.split(" ").foldLeft(List.empty[String]) {(acc, str) =>
      if (str.nonEmpty) {
        str :: acc
      }
      else {
        acc
      }
    }.mkString(" ")
  }

  def removeStones(stones: Array[Array[Int]]): Int = {
    def removeStonesRec(stack: List[Array[Int]] = List.empty
                        , visited: Set[Array[Int]] = Set.empty
                        , curIndex: Int = 0
                        , groupCount: Int = 0): Int = {
      if (stack.isEmpty) {
        if (curIndex == stones.length) stones.length - groupCount
        else if (visited(stones(curIndex))) removeStonesRec(stack, visited, curIndex + 1, groupCount)
        else removeStonesRec(stones(curIndex) :: stack, visited, curIndex + 1, groupCount + 1)
      }
      else {
        val head = stack.head
        val newSet = visited + head
        val toAdd = stones.filter(pair => !newSet(pair) && (pair(0) == head(0) || pair(1) == head(1))).toList
        if (toAdd.nonEmpty) {
          removeStonesRec(toAdd ::: stack.tail, newSet, curIndex, groupCount)
        }
        else {
          removeStonesRec(stack.tail, newSet, curIndex, groupCount)
        }
      }
    }

    removeStonesRec()
  }

  case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }
  def countNodes(root: TreeNode): Int = {
    def counter(node: TreeNode = root): Int = {
      if (node == null) 0
      else {
        1 + counter(node.left) + counter(node.right)
      }
    }

    counter()
  }

  def guess(i: Int): Int= 6 - i
  def guessNumber(n: Int): Int = {
    def guessRec(from: Int = 1, to: Int = n): Int = {
      val middle = from + (to - from) / 2

      guess(middle) match {
        case 0 => middle
        case -1 => guessRec(from, middle)
        case 1 => guessRec(middle + 1, to)
      }
    }

    guessRec()
  }

  def computeArea(ax1: Int, ay1: Int, ax2: Int, ay2: Int, bx1: Int, by1: Int, bx2: Int, by2: Int): Int = {
    val (newX1, newY1) =
      (ax1.max(bx1), ay1.max(by1))

    val (newX2, newY2) =
      (ax2.min(bx2), ay2.min(by2))

    (ay2 - ay1) * (ax2 - ax1) + (by2 - by1) * (bx2 - bx1) - (if (newX2 - newX1 > 0 && newY2 - newY1 > 0) (newY2 - newY1) * (newX2 - newX1) else 0)
  }

  def isUgly(n: Int): Boolean = {
    if (n <= 0) false
    else {
      def divide(prime: Int, curN: Int): Int = {
        if (curN % prime == 0) {
          divide(prime, curN / prime)
        }
        else {
          curN
        }
      }

      List(2, 3, 5).foldLeft(n) { (acc, prime) =>
        divide(prime, acc)
      } == 1
    }
  }

  def search(nums: Array[Int], target: Int): Int = {
    def searchRec(from: Int = 0, to: Int = nums.length - 1): Int = {
      if (from > to) -1
      else {
        val middle = from + (to - from) / 2

        target - nums(middle) match {
          case 0 => middle
          case x if x < 0 => searchRec(from, middle - 1)
          case x if x > 0 => searchRec(middle + 1, to)
        }
      }
    }

    searchRec()
  }

  def isBadVersion(version: Int): Boolean = ???
  def firstBadVersion(n: Int): Int = {
    def searchRec(from: Int = 1, to: Int = n, lastTrueIndex: Int = -1): Int = {
      if (from > to) lastTrueIndex
      else {
        val middle = from + (to - from) / 2

        if (isBadVersion(middle)) {
          searchRec(from, middle - 1, middle)
        } else {
          searchRec(middle + 1, to, lastTrueIndex)
        }
      }
    }

    searchRec()
  }

  def searchInsert(nums: Array[Int], target: Int): Int = {
    nums.search(target) match {
      case InsertionPoint(insertionPoint) => insertionPoint
      case Found(foundIndex) => foundIndex
    }
  }

  def searchInsertByHands(nums: Array[Int], target: Int): Int = {
    def searchRec(from: Int = 0, to: Int = nums.length - 1, lastCheckedIndex: Int = -1): Int = {
      if (from > to) {
        (lastCheckedIndex +
          (if (target - nums(lastCheckedIndex) > 0) -1
          else 1)).max(0)
      }
      else {
        val middle = from + (to - from) / 2

        target - nums(middle) match {
          case 0 => middle
          case x if x < 0 => searchRec(from, middle - 1, middle)
          case x if x > 0 => searchRec(middle + 1, to, middle)
        }
      }
    }

    searchRec()
  }

  def sortedSquares(nums: Array[Int]): Array[Int] = {
    nums.map(Math.pow(_, 2).toInt).sorted
  }

  def rotate(nums: Array[Int], k: Int): Unit = {
    val split = nums.splitAt(nums.length - k)
    val result = split._2 ++ split._1

    nums.indices.foreach(i => nums(i) = result(i))
  }

  def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int = {
    val width = maze(0).length
    val high = maze.length
    val moves = Array(Array(1, 0), Array(-1, 0), Array(0, 1), Array(0, -1))

    def nearestExitRec(curQueue: mutable.Queue[Array[Int]] = mutable.Queue(Array(entrance(0), entrance(1), 0))): Int = {
      if (curQueue.isEmpty) -1
      else {
        val curPos = curQueue.dequeue()

        if (curPos(2) != 0 && (curPos(0) == 0 || curPos(0) == high - 1 || curPos(1) == 0 || curPos(1) == width - 1)) curPos(2)
        else {
          curQueue.enqueueAll(
            moves
              .withFilter(move =>
                (move(0) + curPos(0)) < high &&
                (move(0) + curPos(0)) >= 0 &&
                (move(1) + curPos(1)) < width &&
                (move(1) + curPos(1)) >= 0 &&
                maze(move(0) + curPos(0))(move(1) + curPos(1)) == '.')
              .map(move => Array(move(0) + curPos(0), move(1) + curPos(1), curPos(2) + 1))
          )

          maze(curPos(0))(curPos(1)) = '+'

          nearestExitRec(curQueue)
        }
      }
    }

    nearestExitRec()
  }

  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    def twoSumRec(left: Int = 0, right: Int = numbers.length - 1): Array[Int] = {
      if (left == numbers.length - 1) Array.empty
      else {
        if (left == right) {
          LazyList.range(left + 1, numbers.length, 1).find(newLeft => numbers(newLeft) != numbers(left)) match {
            case Some(l) => twoSumRec(l)
            case None => Array.empty
          }
        }
        else if (numbers(left) + numbers(right) == target) Array(left + 1, right + 1)
        else {
          LazyList.range(right - 1, left, -1).find(newRight => numbers(right) != numbers(newRight)) match {
            case Some(r) => twoSumRec(left, r)
            case _ => twoSumRec(left, left)
          }
        }
      }
    }

    twoSumRec()
  }

  def moveZeroes(nums: Array[Int]): Unit = {
    nums.indices.foldLeft(0) { (acc, index) =>
      if (nums(index) == 0) {
        acc + 1
      }
      else {
        if (acc != 0) {
          nums(index - acc) = nums(index)
          nums(index) = 0
        }
        acc
      }
    }
  }

  def reverseString(s: Array[Char]): Unit = {
    def reverseRec(left: Int, right: Int): Unit = {
      if (left >= right) ()
      else if (s(left) == s(right)) reverseRec(left + 1, right - 1)
      else {
        val memo = s(left)
        s(left) = s(right)
        s(right) = memo

        reverseRec(left + 1, right - 1)
      }
    }

    reverseRec(0, s.length - 1)
  }

  def reverseWordsInside(s: String): String = {
    s.split(" ").view.map(_.reverse).mkString(" ")
  }

  def numSquares(n: Int): Int = {
    val arrOfSolutions = Array.fill(n + 1)(Int.MaxValue)
    arrOfSolutions(0) = 0
    val numSquares = Math.sqrt(n).toInt
    (1 to numSquares).foreach { index =>
      arrOfSolutions(index * index) = 1
    }

    (1 to n).foreach { index =>
      (0 to index / 2).foreach { anotherIndex =>
        arrOfSolutions(index) = Math.min(arrOfSolutions(index), arrOfSolutions(anotherIndex) + arrOfSolutions(index - anotherIndex))
      }
    }

    arrOfSolutions(n)
  }

  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    def recSol(row: Int): Boolean = {
      if (row > 8) true
      else {
        //check the line
        if (!isValid(board(row))) false
        else if (
          !isValid(
            for {
              line <- board
            } yield line(row))) false
        else if (!isValid(
          board(row / 3 * 3).slice(row % 3 * 3, row % 3 * 3 + 3) ++
          board(row / 3 * 3 + 1).slice(row % 3 * 3, row % 3 * 3 + 3) ++
          board(row / 3 * 3 + 2).slice(row % 3 * 3, row % 3 * 3 + 3))) false
        else
          recSol(row + 1)
      }
    }

    def isValid(line: Array[Char], index: Int = 0): Boolean = {
      if (index > 7) true
      else {
        if ((index + 1 to 8).exists(checkIndex => line(index) != '.' && line(checkIndex) != '.' && line(index) == line(checkIndex))) false
        else isValid(line, index + 1)
      }
    }

    recSol(0)
  }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def middleNode(head: ListNode): ListNode = {
    def length(curNode: ListNode = head, finalLength: Int = 0): Int = {
      Option(curNode) match {
        case Some(n) => length(n.next, finalLength + 1)
        case None => finalLength
      }
    }

    def getMiddle(curNode: ListNode = head, steps: Int): ListNode = {
      if (steps > 0) {
        getMiddle(curNode.next, steps - 1)
      }
      else {
        curNode
      }
    }
    val listLength = length()

    if (listLength % 2 == 1) {
      getMiddle(steps = listLength / 2)
    }
    else {
      getMiddle(steps = listLength / 2 + 1)
    }
  }

  def middleNodeFastSlow(head: ListNode): ListNode = {
    def fastSlow(fast: ListNode, slow: ListNode): ListNode = {
      Option(fast) match {
        case Some(faster) =>
          Option(faster.next) match {
            case Some(fastest) => fastSlow(fastest, slow.next)
            case None => slow
          }
        case None => slow
      }
    }

    fastSlow(head, head)
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    def removeAtRec(rem: ListNode, result: ListNode, steps: Int = length() - n): ListNode = {
      if (steps == 0) {
        rem.next
      }
      else if (steps == 1) {
        result.next = Option(rem.next).map(_.next).orNull

        head
      }
      else {
        removeAtRec(rem.next, result.next, steps - 1)
      }
    }

    def length(curNode: ListNode = head, finalLength: Int = 0): Int = {
      Option(curNode) match {
        case Some(n) => length(n.next, finalLength + 1)
        case None => finalLength
      }
    }

    removeAtRec(head, head)
  }

  def lengthOfLongestSubstring(s: String): Int = {
    val strLength = s.length

    def lengthRec(left: Int = 0, right: Int = 1, visited: Set[Char] = Set.empty, res: Int = 1): Int = {
      if (left >= strLength - 1) res
      else if (right >= strLength) res.max(visited.size)
      else {
        if (!visited(s(right))) {
          val newVisited = visited + s(right)

          lengthRec(left, right + 1, newVisited, res)
        }
        else {
          lengthRec(left + 1, left + 2, Set.empty, res.max(visited.size))
        }
      }
    }
    lengthRec()
  }

  def checkInclusion(s1: String, s2: String): Boolean = {
    val s1Length = s1.length
    val s2Length = s2.length

    val s1Counter = Array.fill(26)(0)

    s1.foreach{ char => s1Counter(char - 'a') += 1}
    def checkInclusionRec(s2Counter: Array[Int] = Array.fill(26)(0), curS1Index: Int = 0, curS2Index: Int = 0): Boolean = {
      if (curS2Index == s2Length - s1Length + 1) false
      else if (curS1Index == s1Length && (s1Counter sameElements s2Counter)) true
      else {
        if (curS1Index == s1Length) {
          s2Counter.indices.foreach(s2Counter(_) = 0)

          checkInclusionRec(s2Counter, 0, curS2Index + 1)
        }
        else {
          s2Counter(s2(curS2Index + curS1Index) - 'a') += 1

          checkInclusionRec(s2Counter, curS1Index + 1, curS2Index)
        }
      }
    }

    checkInclusionRec()
  }

  def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, color: Int): Array[Array[Int]] = {
    val rows = image.length
    val columns = image(0).length
    val startingColor = image(sr)(sc)

    def floodFillRec(stack: List[(Int, Int)]): Unit = {
      if (stack.nonEmpty) {
        val (i, j) = stack.head

        image(i)(j) = color

        floodFillRec(getConnected(i, j) ::: stack.tail)
      }
    }

    def getConnected(i: Int, j: Int): List[(Int, Int)] = {
      val listBuf = ListBuffer[(Int, Int)]()
      if (i - 1 >= 0 && image(i - 1)(j) == startingColor) {
        listBuf += ((i - 1, j))
      }
      if (j + 1 < columns && image(i)(j + 1) == startingColor) {
        listBuf += ((i, j + 1))
      }
      if (j - 1 >= 0 && image(i)(j - 1) == startingColor) {
        listBuf += ((i, j - 1))
      }
      if (i + 1 < rows && image(i + 1)(j) == startingColor) {
        listBuf += ((i + 1, j))
      }

      listBuf.toList
    }

    if (image(sr)(sc) != color) {
      floodFillRec(List((sr, sc)))
    }

    image
  }

  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val columns = grid(0).length

    def maxAreaOfIslandRec(curRow: Int = 0, curColumn: Int = 0, visited: Set[String] = Set.empty, curMax: Int = 0): Int = {
      if (curRow == rows) {
        curMax
      }
      else if (curColumn == columns) {
        maxAreaOfIslandRec(curRow + 1, 0, visited, curMax)
      }
      else {
        if (grid(curRow)(curColumn) == 1 && !visited(s"$curRow/$curColumn")) {
          val (newVisited, curArea) = bfs(visited, List((curRow, curColumn)))

          maxAreaOfIslandRec(curRow, curColumn + 1, newVisited, curMax.max(curArea))
        }
        else {
          maxAreaOfIslandRec(curRow, curColumn + 1, visited, curMax)
        }
      }
    }

    def bfs(visited: Set[String], stack: List[(Int, Int)], area: Int = 0): (Set[String], Int) = {
      if (stack.isEmpty) (visited, area)
      else {
        val (i, j) = stack.head
        if (!visited(s"$i/$j")) {
          val listBuf = ListBuffer[(Int, Int)]()
          if (i - 1 >= 0 && grid(i - 1)(j) == 1 && !visited(s"${(i - 1)}/$j")) {
            listBuf += ((i - 1, j))
          }
          if (j + 1 < columns && grid(i)(j + 1) == 1 && !visited(s"$i/${j + 1}")) {
            listBuf += ((i, j + 1))
          }
          if (j - 1 >= 0 && grid(i)(j - 1) == 1 && !visited(s"$i/${j - 1}")) {
            listBuf += ((i, j - 1))
          }
          if (i + 1 < rows && grid(i + 1)(j) == 1 && !visited(s"${(i + 1)}/$j")) {
            listBuf += ((i + 1, j))
          }

          bfs(visited + s"$i/$j", listBuf.toList ::: stack.tail, area + 1)
        }
        else {
          bfs(visited, stack.tail, area)
        }
      }
    }

    maxAreaOfIslandRec()
  }

  def mergeTrees(root1: TreeNode, root2: TreeNode): TreeNode = {
    (Option(root1), Option(root2)) match {
      case (Some(r1), Some(r2)) =>
        new TreeNode(_value = r1.value + r2.value, _left = mergeTrees(r1.left, r2.left), _right = mergeTrees(r1.right, r2.right))
      case (Some(r1), _) =>
        r1
      case (_, Some(r2)) =>
        r2
      case _ =>
        null
    }
  }

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    if (list1 == null) list2
    else if (list2 == null) list1
    else if (list1.x < list2.x) {
      list1.next = mergeTwoLists(list1.next, list2)
      list1
    }
    else {
      list2.next = mergeTwoLists(list1, list2.next)
      list2
    }
  }

  def reverseList(head: ListNode): ListNode = {
    def reverseListRec(curHead: ListNode, acc: ListNode): ListNode = {
      if (curHead == null) {
        acc
      }
      else {
        reverseListRec(curHead.next, new ListNode(curHead.x, acc))
      }
    }

    reverseListRec(head, null)
  }

  def climbStairs(n: Int): Int = {
    def rec(steps: Int = 0, first: Int = 0, second: Int = 1): Int = {
      if (steps == n) second
      else rec(steps, second, first + second)
    }

    rec(n)
  }

  def halvesAreAlike(s: String): Boolean = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    val (str1, str2) = s.toLowerCase.splitAt(s.length / 2)

    str1.count(vowels.contains) == str2.count(vowels.contains)
  }

  def isPowerOfTwo(n: Int): Boolean = {
    if (n <= 0) false
    else n.toBinaryString.count(_ == '1') == 1
  }

  def reverseBits(x: Int): Int = {
    val padded = x.toBinaryString.reverse.padTo(32, '0')
    java.lang.Long.parseLong(padded, 2).toInt
  }

  def singleNumber(nums: Array[Int]): Int = {
    nums.foldLeft((Set.empty[Int], 0)) {(acc, cur) =>
      if (acc._1(cur)) {
        (acc._1, acc._2 - cur)
      }
      else {
        (acc._1 + cur, acc._2 + cur)
      }
    }._2
  }

  def frequencySort(s: String): String = {
    s
      .toSeq
      .groupBy(identity)
      .toList
      .view
      .map(pair => (pair._1, pair._2.length))
      .sortBy(pair => (-pair._2, pair._1))
      .map(pair => pair._1.toString * pair._2)
      .mkString
  }

  def minimumAverageDifference(nums: Array[Int]): Int = {
    def getMapResult(map: Map[Int, (Long, Long)], index: Int): Map[Int, (Long, Long)] = {
      if (index == nums.length) map
      else if (index == 0) {
        getMapResult(map + (index -> (nums(index), nums.map(_.toLong).sum - nums(index))), index + 1)
      }
      else {
        val (prevLeft, prevRight) = map(index - 1)
        getMapResult(map + (index -> (prevLeft + nums(index), prevRight - nums(index))), index + 1)
      }
    }

    def rec(curIndex: Int, mapRes: Map[Int, (Long, Long)], curMin: Option[(Int, Long)]): Int = {
      if (curIndex == nums.length) curMin.get._1
      else {
        val (resLef, resRight) = mapRes(curIndex)
        val finalRes = Math.abs(resLef / (curIndex + 1) - (resRight / (if (curIndex == (nums.length - 1)) 1 else nums.length - 1 - curIndex)))

        curMin match {
          case Some((_, minRes)) =>
            if (minRes > finalRes) {
              rec(curIndex + 1, mapRes, Some(curIndex, finalRes))
            }
            else {
              rec(curIndex + 1, mapRes, curMin)
            }
          case None =>
            rec(curIndex + 1, mapRes, Some(curIndex, finalRes))
        }
      }
    }

    rec(0, getMapResult(Map.empty, 0), None)
  }

  def altMinAvg(nums: Array[Int]): Int = {

    val length = nums.length

    if (length == 0) 0
    else {
      nums
        .scanLeft((0L, nums.map(_.toLong).sum))((acc, num) => (acc._1 + num, acc._2 - num))
        .zipWithIndex
        .tail
        .map(pair => (Math.abs(pair._1._1 / pair._2 - (if (pair._2 == length) 0 else pair._1._2 / (length - pair._2))), pair._2 - 1))
        .min
        ._2
    }
  }

  class MyQueue() {
    private var enqueueList = List.empty[Int]
    private var dequeueList = List.empty[Int]

    def push(x: Int) {
      enqueueList = x :: enqueueList
    }

    def pop(): Int = {
      dequeueList match {
        case h :: t =>
          dequeueList = t
          h
        case Nil =>
          swap()
          pop()
      }
    }

    private def swap(): Unit = {
      dequeueList = enqueueList.reverse
      enqueueList = List.empty
    }

    def peek(): Int = {
      dequeueList match {
        case h :: _ =>
          h
        case Nil =>
          swap()
          peek()
      }
    }

    def empty(): Boolean = {
      dequeueList.isEmpty && enqueueList.isEmpty
    }
  }

  def evalRPN(tokens: Array[String]): Int = {
    tokens.foldLeft(List.empty[Int]) {
      case (secondInt :: firstInt :: remStack, "*") => (firstInt * secondInt) :: remStack
      case (secondInt :: firstInt :: remStack, "/") => (firstInt / secondInt) :: remStack
      case (secondInt :: firstInt :: remStack, "+") => (firstInt + secondInt) :: remStack
      case (secondInt :: firstInt :: remStack, "-") => (firstInt - secondInt) :: remStack
      case (stack, nextNumber) => nextNumber.toInt :: stack
    }.head
  }

  def dailyTemperatures(temperatures: Array[Int]): Array[Int] = {
    val result = Array.fill(temperatures.length)(0)

    @tailrec
    def checkTemperature(stack: List[Int], indexForCheck: Int): List[Int] = {
      stack match {
        case nextIndex :: t =>
          if (temperatures(nextIndex) < temperatures(indexForCheck)) {
            result(nextIndex) = indexForCheck - nextIndex

            checkTemperature(t, indexForCheck)
          }
          else indexForCheck :: stack
        case Nil => indexForCheck :: stack
      }
    }

    temperatures.indices.foldLeft(List.empty[Int]) {
      case (acc, nextIndex) =>
        acc match {
          case Nil => nextIndex :: acc
          case _ => checkTemperature(acc, nextIndex)
        }
    }

    result
  }

  def validPath(n: Int, edges: Array[Array[Int]], source: Int, destination: Int): Boolean = {
    val graph = edges.foldLeft(Map.empty[Int, List[Int]]) {(acc, nextEdge) =>
      val (leftVertex, rightVertex) = (nextEdge(0), nextEdge(1))
      val leftConnections = acc.getOrElse(leftVertex, List())
      val rightConnections = acc.getOrElse(rightVertex, List())

      acc.updated(leftVertex, rightVertex :: leftConnections).updated(rightVertex, leftVertex :: rightConnections)
    }

    @tailrec
    def rec(queue: Queue[Int], visited: Array[Boolean]): Boolean = {
      if (queue.isEmpty) false
      else {
        val (intForCheck, remQueue) = queue.dequeue

        if (intForCheck == destination) true
        else if (visited(intForCheck)) rec(remQueue, visited)
        else {
          visited(intForCheck) = true
          rec(remQueue.enqueueAll(graph.getOrElse(intForCheck, List())), visited)
        }
      }
    }

    rec(Queue(source), Array.fill(n)(false))
  }

  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    val arrayRooms = rooms.toArray

    @tailrec
    def rec(queue: Queue[Int], visited: Array[Boolean]): Boolean = {
      if (queue.isEmpty) visited.forall(identity)
      else {
        val (room, remQueue) = queue.dequeue

        if (visited(room)) rec(remQueue, visited)
        else {
          visited(room) = true
          rec(remQueue.enqueueAll(arrayRooms(room)), visited)
        }
      }
    }

    rec(Queue(0), Array.fill(arrayRooms.length)(false))
  }

  def possibleBipartition(n: Int, dislikes: Array[Array[Int]]): Boolean = {
    val graph = dislikes.foldLeft(Map.empty[Int, List[Int]]) { (acc, nextEdge) =>
      val (leftVertex, rightVertex) = (nextEdge(0) - 1, nextEdge(1) - 1)
      val leftConnections = acc.getOrElse(leftVertex, List())
      val rightConnections = acc.getOrElse(rightVertex, List())

      acc.updated(leftVertex, rightVertex :: leftConnections).updated(rightVertex, leftVertex :: rightConnections)
    }

    @tailrec
    def rec(queue: Queue[Int], edgesStack: List[Int], visited: Array[Boolean], color: Array[Int]): Boolean = {
      if (queue.isEmpty && edgesStack.isEmpty) true
      else if (queue.isEmpty) {
        edgesStack.dropWhile(visited) match {
          case h :: t => rec(queue.enqueue(h), t, visited, color)
          case Nil => true
        }
      }
      else {
        val (person, remQueue) = queue.dequeue

        if (visited(person)) rec(remQueue, edgesStack, visited, color)
        else {
          val curColor =
            if (color(person) == -1) {
              color(person) = 0
              0
            }
            else color(person)
          val curDislikes = graph(person)
          val (colored, notColored) = curDislikes.partition(color(_) != -1)

          if (colored.exists(color(_) == curColor)) false
          else {
            notColored.foreach(color(_) = 1 - curColor)

            visited(person) = true
            rec(remQueue.enqueueAll(curDislikes), edgesStack, visited, color)
          }
        }
      }
    }

    graph.headOption match {
      case Some((person, _)) => rec(Queue(person), graph.keySet.toList, Array.fill(n)(false), Array.fill(n)(-1))
      case None => true
    }
  }

  def minStoneSum(piles: Array[Int], k: Int): Int = {
    (0 until k).foldLeft(mutable.PriorityQueue(piles: _*))((queue, _) => queue.addOne(queue.head - queue.dequeue() / 2)).sum
  }

  def wordPattern(pattern: String, s: String): Boolean = {
    val allWords = s.split(" ")

    @tailrec
    def rec(curIndex: Int, map: Map[Char, String], visited: Set[String]): Boolean = {
      if (curIndex == pattern.length) true
      else {
        val nextPattern = pattern(curIndex)
        val nextWordToCheck = allWords(curIndex)

        if (map.contains(nextPattern)) {
          if (map(nextPattern) == nextWordToCheck) rec(curIndex + 1, map, visited)
          else false
        }
        else if (visited(nextWordToCheck)) false
        else rec(curIndex, map + (nextPattern -> nextWordToCheck), visited + nextWordToCheck)
      }
    }

    if (pattern.length != allWords.length) false
    else rec(0, Map.empty, Set.empty)
  }

  def detectCapitalUse(word: String): Boolean = {
    word.matches("""[A-Z]*|.[a-z]*""")
  }

  def minDeletionSize(strs: Array[String]): Int = {
    val stringLength = strs(0).length

    @tailrec
    def rec(index: Int, countToDel: Int): Int = {
      if (index == stringLength) countToDel
      else {
        val stringToCheck = strs.map(_(index)).mkString

        if (stringToCheck == stringToCheck.sorted) rec(index + 1, countToDel)
        else rec(index + 1, countToDel + 1)
      }
    }

    rec(0, 0)
  }

  def minimumRounds(tasks: Array[Int]): Int = {
    val counts = tasks.groupMapReduce(identity)(_ => 1)(_ + _).values.toList

    @tailrec
    def rec(remList: List[Int], acc: Int): Int = {
      remList match {
        case h :: t =>
          if (h == 1) -1
          else if (h % 3 == 0) rec(t, acc + h / 3)
          else rec(t, acc + h / 3 + 1)
        case Nil => acc
      }
    }

    rec(counts, 0)
  }

  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    val sortedPoints = points.sortBy(_(0))

    @tailrec
    def rec(curIndex: Int, mutualStarZone: Int, mutualEndZone: Int, arrowCounter: Int): Int = {
      if (curIndex == points.length) arrowCounter
      else {
        val (checkStart, checkEnd) = (sortedPoints(curIndex)(0), sortedPoints(curIndex)(1))

        if (checkStart <= mutualEndZone) {
          val newStartZone = if (checkStart > mutualStarZone) checkStart else mutualStarZone
          val newEndZone = if (checkEnd < mutualEndZone) checkEnd else mutualEndZone

          rec(curIndex + 1, newStartZone, newEndZone, arrowCounter)
        }
        else {
          rec(curIndex + 1, checkStart, checkEnd, arrowCounter + 1)
        }
      }
    }

    rec(1, sortedPoints(0)(0), sortedPoints(0)(1), 1)
  }

  def maxIceCream(costs: Array[Int], coins: Int): Int = {
    val sortedCosts = costs.sorted

    @tailrec
    def rec(curIndex: Int, curCoinsAmount: Int): Int = {
      if (curIndex == costs.length || sortedCosts(curIndex) > curCoinsAmount) curIndex
      else rec(curIndex + 1, curCoinsAmount - sortedCosts(curIndex))
    }

    rec(0, coins)
  }

  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
    val (total, _ , start) = gas.zip(cost).map(pair => pair._1 - pair._2).zipWithIndex.foldLeft((0, 0, 0)){
      case ((totalRemGas, curRemGas, start), (gasDiff, index)) =>
        if (curRemGas < 0) {
          (totalRemGas + gasDiff, gasDiff, index)
        }
        else {
          (totalRemGas + gasDiff, curRemGas + gasDiff, start)
        }
    }

    if (total < 0) -1
    else start
  }

  def preorderTraversal(root: TreeNode): List[Int] = {
    @tailrec
    def rec(stack: List[TreeNode], acc: List[Int]): List[Int] = {
      stack match {
        case h :: t =>
          if (h != null) rec(h.left :: h.right :: t, h.value :: acc)
          else rec(t, acc)
        case Nil =>
          acc.reverse
      }
    }

    rec(List(root), List.empty)
  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    @tailrec
    def rec(stackP: List[TreeNode], stackQ: List[TreeNode]): Boolean = {
      (stackP, stackQ) match {
        case (Nil, Nil) => true
        case (_, Nil) | (Nil, _) => false
        case (ph :: pt, qh :: qt) =>
          if (isEqualValues(ph, qh)) rec(getNewStack(ph, pt), getNewStack(qh, qt))
          else false
      }
    }

    def isEqualValues(pNode: TreeNode, qNode: TreeNode): Boolean = {
      (Option(pNode), Option(qNode)) match {
        case (None, None) => true
        case (None, _) | (_, None) => false
        case _ => pNode.value == qNode.value
      }
    }

    def getNewStack(node: TreeNode, stack: List[TreeNode]): List[TreeNode] = {
      Option(node) match {
        case Some(value) => value.left :: value.right :: stack
        case None => stack
      }
    }

    rec(List(p), List(q))
  }
}
