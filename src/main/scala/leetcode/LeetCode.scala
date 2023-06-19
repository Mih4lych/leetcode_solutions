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

  case class ListNode(_x: Int = 0, _next: ListNode = null) {
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

  def minFlipsMonoIncr(s: String): Int = {
    s.dropWhile(_ == '0').foldLeft((0, 0)) { case ((num, acc), nextChar)  =>
       if (nextChar == '0') (num, num.min(acc + 1))
       else (num + 1, acc)
    }._2
  }

  def subarraysDivByK(nums: Array[Int], k: Int): Int = {
    @tailrec
    def rec(leftIndex: Int, rightIndex: Int, curSum: Int, res: Int): Int = {
      if (leftIndex == nums.length) res
      else if (rightIndex == nums.length) rec(leftIndex + 1, leftIndex + 1, 0, res)
      else {
        val newSum = curSum + nums(rightIndex)
        if (newSum % k == 0) rec(leftIndex, rightIndex + 1, newSum, res + 1)
        else rec(leftIndex, rightIndex + 1, newSum, res)
      }
    }

    rec(0, 0, 0, 0)
  }

  def anotherSubarraysDivByK(nums: Array[Int], k: Int): Int = {
    nums.foldLeft((Array.fill(k)(0).updated(0, 1), 0, 0)) {
      case ((modules, curMod, res), nextNum) =>
        val newMod = (curMod + nextNum % k + k) % k
        val newRes = res + modules(newMod)
        modules(newMod) += 1
        (modules, newMod, newRes)
    }._3
  }

  def findJudge(n: Int, trust: Array[Array[Int]]): Int = {
    val (trustCount, trusting) =
      trust.foldLeft((Array.fill(n)(0), Array.fill(n)(false))) { case ((curCount, curTrusting), trusting) =>
        curCount(trusting(1) - 1) += 1
        curTrusting(trusting(0) - 1) = true
        (curCount, curTrusting)
      }

    trustCount
      .zip(trusting)
      .zipWithIndex
      .find{ case ((count, trusting), _) => count == n - 1 && !trusting} match {
        case Some((_, index)) => index + 1
        case None => -1
      }
  }

  def findJudge2(n: Int, trust: Array[Array[Int]]): Int = {
    trust
      .foldLeft(Array.fill(n)(0)) { (acc, trusting) =>
        acc(trusting(0) - 1) -= 1
        acc(trusting(1) - 1) += 1

        acc
      }
      .zipWithIndex
      .find(_._1 == n - 1) match {
        case Some((_, index)) => index + 1
        case None => -1
      }
  }

  def closestMeetingNode(edges: Array[Int], node1: Int, node2: Int): Int = {
    def rec(nextNode: Int, curDist: Int, acc: Array[Int], visited: Array[Boolean]): Array[Int] = {
      if (edges(nextNode) == -1 || visited(edges(nextNode))) acc.updated(nextNode, curDist)
      else rec(edges(nextNode), curDist + 1, acc.updated(nextNode, curDist), visited.updated(nextNode, true))
    }

    rec(node1, 0, Array.fill(edges.length)(Int.MaxValue).updated(node1, 0), Array.fill(edges.length)(false))
      .lazyZip(rec(node2, 0, Array.fill(edges.length)(Int.MaxValue).updated(node2, 0), Array.fill(edges.length)(false)))
      .zipWithIndex
      .foldLeft((-1, Int.MaxValue)) { case ((minIndex, minDist), ((dist1, dist2), curInd)) =>
        if (minDist > dist1.max(dist2)) (curInd, dist1.max(dist2))
        else (minIndex, minDist)
      }._1
  }

  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    @tailrec
    def loop(nums1Index: Int, nums2Index: Int, num1Ending: Int): Unit = {
      if (nums2Index < 0) ()
      else if (num1Ending >= 0 && nums1(nums1Index) > nums2(nums2Index)) {
        nums1(num1Ending) = nums1(nums1Index)
        loop(nums1Index - 1, nums2Index, num1Ending - 1)
      }
      else {
        nums1(num1Ending) = nums2(nums2Index)
        loop(nums1Index, nums2Index - 1, num1Ending - 1)
      }
    }

    loop(m - 1, n - 1, nums1.length - 1)
  }

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    @tailrec
    def loop(curIndex: Int, newEnding: Int): Int = {
      if (curIndex == newEnding) newEnding
      else if (nums(curIndex) != `val`) loop(curIndex + 1, newEnding)
      else {
        nums(curIndex) = nums(newEnding - 1)
        loop(curIndex, newEnding - 1)
      }
    }

    loop(0, nums.length)
  }

  def removeDuplicatesDouble(nums: Array[Int]): Int = {
    @tailrec
    def loop(curIndex: Int, newArrayIndex: Int, duplicates: Int): Int = {
      if (curIndex >= nums.length) newArrayIndex + 1
      else if (nums(curIndex) == nums(newArrayIndex)) {
        if (duplicates == 2) loop(curIndex + 1, newArrayIndex, duplicates)
        else {
          nums(newArrayIndex + 1) = nums(curIndex)
          loop(curIndex + 1, newArrayIndex + 1, duplicates + 1)
        }
      }
      else {
        nums(newArrayIndex + 1) = nums(curIndex)
        loop(curIndex + 1, newArrayIndex + 1, 1)
      }
    }

    loop(1, 0, 1)
  }

  def removeDuplicatesSimple(nums: Array[Int]): Int = {
    nums.foldLeft(0){ (index, num) =>
      if (index < 2 || num != nums(index - 2)) {
        nums(index) = num
        index + 1
      } else index
    }
  }

  def uniquePaths(obstacles: Array[Array[Int]], m: Int, n: Int): Int = {
    def loop(i: Int, j: Int, paths: Array[Array[Int]]): Int = {
      if (i == m - 1 && j == n) paths(m - 1)(n - 1)
      else if (j == n) loop(i + 1, 0, paths)
      else if (obstacles(i)(j) == 1) loop(i, j + 1, paths)
      else if (i > 0 && j > 0) {
        paths(i)(j) = paths(i)(j - 1) + paths(i - 1)(j)
        loop(i, j + 1, paths)
      }
      else if (i == 0) {
        paths(i)(j) = paths(i)(j - 1)
        loop(i, j + 1, paths)
      }
      else {
        paths(i)(j) = paths(i - 1)(j)
        loop(i, j + 1, paths)
      }
    }

    val path = Array.fill(m, n)(0)
    path(0)(0) = 1
    loop(0, 1, path)
  }

  def climbStairsNew(n: Int): Int = {
    def loop(first: Int, second: Int, curStep: Int): Int = {
      if (curStep > n) second
      else loop(second, first + second, curStep + 1)
    }

    loop(0, 1, 1)
  }

  def rob(nums: Array[Int]): Int = {
    @tailrec
    def loop(firstPathProfit: Int, secondPathProfit: Int, curIndex: Int): Int = {
      if (curIndex == nums.length) secondPathProfit
      else {
        val newFinalProfit = (firstPathProfit + nums(curIndex)).max(secondPathProfit)
        loop(secondPathProfit, newFinalProfit, curIndex + 1)
      }
    }

    if (nums.length == 1) nums(0)
    else if (nums.length == 2) nums(0).max(nums(1))
    else loop(nums(0), nums(0).max(nums(1)), 2)
  }

  def containsDuplicate(nums: Array[Int]): Boolean = {
    @tailrec
    def loop(index: Int, setUnique: Set[Int]): Boolean = {
      if (index == nums.length) false
      else if (setUnique.contains(nums(index))) true
      else loop(index + 1, setUnique + nums(index))
    }

    loop(0, Set.empty[Int])
  }

  def missingNumber(nums: Array[Int]): Int = {
    val length = nums.length + 1

    (length * (length - 1)) / 2 - nums.sum
  }

  def maxProfit(prices: Array[Int]): Int = {
    prices.foldLeft(Int.MaxValue, 0) { case ((minPrice, maxProfit), nextPrice) =>
      val newMin = minPrice.min(nextPrice)

      (newMin, maxProfit.max(nextPrice - newMin))
    }._2
  }

  def maxSubArray(nums: Array[Int]): Int = {
    nums.foldLeft((Int.MinValue, 0)) { case ((curMax, curSum), nextInt) =>
      val newSum = nextInt.max(curSum + nextInt)

      (curMax.max(newSum), newSum)
    }._1
  }

  def countBits(n: Int): Array[Int] = {
    (0 to n).foldLeft(new Array[Int](n + 1)) { (array, num) =>
      array(num) = num.toBinaryString.count(_ == '1')

      array
    }
  }

  def countBitsDP(n: Int): Array[Int] = {
    (0 to n).foldLeft(new Array[Int](n + 1)) { (array, num) =>
      array(num) = array(num >> 1) + num % 2

      array
    }
  }

  def hasCycle(head: ListNode): Boolean = {
    @tailrec
    def loop(slow: ListNode, fast: ListNode): Boolean = {
      if (slow.next == null || fast.next == null || fast.next.next == null) false
      else if (slow == fast.next.next) true
      else loop(slow.next, fast.next.next)
    }

    loop(head, head)
  }

  //[4,3,2,7,8,2,3,1]
  def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
    @tailrec
    def loop(index: Int): List[Int] = {
      if (index == nums.length) getMissed
      else {
        val pos = nums(index)

        if (nums(pos - 1) != pos) {
          nums(index) = nums(pos - 1)
          nums(pos - 1) = pos

          loop(index)
        }
        else loop(index + 1)
      }
    }

    def getMissed: List[Int] =
      nums.indices.foldLeft(List.empty[Int])((list, nextIndex) => if (nums(nextIndex) != nextIndex + 1) nextIndex + 1 :: list else list).reverse

    loop(0)
  }

  def climbStairsFold(n: Int): Int = {
    (0 to n).foldLeft((1, 1)) { case ((firstSolution, secondSolution), _) => (secondSolution, firstSolution + secondSolution) }._2
  }

  def reverseListWithoutCreating(head: ListNode): ListNode = {
    @tailrec
    def reverseListRec(curHead: ListNode, acc: ListNode): ListNode = {
      if (curHead == null) {
        acc
      }
      else {
        val nextHead = curHead.next
        curHead.next = acc
        reverseListRec(nextHead, curHead)
      }
    }

    reverseListRec(head, null)
  }

  def isPalindrome(head: ListNode): Boolean = {
    def loop(slow: ListNode, fast: ListNode, reversedList: ListNode): Boolean = {
      if (slow == null) false
      else if (fast == null) checkPalindrome(reversedList, slow)
      else if (fast.next == null) checkPalindrome(reversedList, slow.next)
      else loop(slow.next, fast.next.next, new ListNode(slow.x, reversedList))
    }

    def checkPalindrome(firstPart: ListNode, secondPart: ListNode): Boolean = {
      if (firstPart == null) true
      else if (firstPart.x != secondPart.x) false
      else checkPalindrome(firstPart.next, secondPart.next)
    }

    loop(head, head, null)
  }

  def isPalindromeWithoutCreating(head: ListNode): Boolean = {
    @tailrec
    def loop(slow: ListNode, fast: ListNode, reversedList: ListNode): Boolean = {
      if (slow == null) false
      else if (fast == null) checkPalindrome(reversedList, slow)
      else if (fast.next == null) checkPalindrome(reversedList, slow.next)
      else {
        val newSlow = slow.next
        val newFast = fast.next.next
        slow.next = reversedList

        loop(newSlow, newFast, slow)
      }
    }

    @tailrec
    def checkPalindrome(firstPart: ListNode, secondPart: ListNode): Boolean = {
      if (firstPart == null) true
      else if (firstPart.x != secondPart.x) false
      else checkPalindrome(firstPart.next, secondPart.next)
    }

    loop(head, head, null)
  }

  def removeElements(head: ListNode, `val`: Int): ListNode = {
    @tailrec
    def loop(curNode: ListNode): Unit = {
      if (curNode.next == null) ()
      else if (curNode.next.x == `val`) {
        curNode.next = curNode.next.next

        loop(curNode)
      }
      else {
        loop(curNode.next)
      }
    }

    val newList = new ListNode(0, head)

    loop(newList)

    newList.next
  }

  def deleteDuplicates(head: ListNode): ListNode = {
    @tailrec
    def loop(curNode: ListNode): ListNode = {
      if (curNode.next == null) head
      else if (curNode.x == curNode.next.x) {
        curNode.next = curNode.next.next

        loop(curNode)
      }
      else loop(curNode.next)
    }

    if (head == null) head
    else loop(head)
  }

  def mergeTwoListsRec(list1: ListNode, list2: ListNode): ListNode = {
    @tailrec
    def loop(curNode1: ListNode, curNode2: ListNode, curNodeResult: ListNode): Unit = {
      if (curNode1 == null) curNodeResult.next = curNode2
      else if (curNode2 == null) curNodeResult.next = curNode1
      else {
        if (curNode1.x > curNode2.x) {
          curNodeResult.next = curNode2
          loop(curNode1, curNode2.next, curNodeResult.next)
        }
        else {
          curNodeResult.next = curNode1
          loop(curNode1.next, curNode2, curNodeResult.next)
        }
      }
    }

    val result = new ListNode(0, null)
    loop(list1, list2, result)

    result.next
  }

  class Interval(_start: Int = 0, _end: Int = 0) {
    var start: Int = _start
    var end: Int = _end
  }

  def canAttendMeetings(intervals: Array[Interval]): Boolean = {
    @tailrec
    def loop(index: Int, sortedIntervals: Array[Interval]): Boolean = {
      if (index == sortedIntervals.length - 1) true
      else if (sortedIntervals(index).end > sortedIntervals(index + 1).start) false
      else loop(index + 1, sortedIntervals)
    }

    loop(0, intervals.sortBy(_.start))
  }

  def nextGreatestLetter(letters: Array[Char], target: Char): Char = {
    @tailrec
    def loop(from: Int, to: Int, lastTargetChar: Option[Char]): Char = {
      if (from > to) lastTargetChar match {
        case Some(value) => value
        case None => letters(0)
      }
      else {
        val middle = from + (to - from) / 2

        if (letters(middle) <= target) loop(middle + 1, to, lastTargetChar)
        else {
          lastTargetChar match {
            case Some(value) => loop(from, middle - 1, Some(if (value < letters(middle)) value else letters(middle)))
            case None => loop(from, middle - 1, Some(letters(middle)))
          }
        }
      }
    }

    loop(0, letters.length - 1, None)
  }

  def peakIndexInMountainArray(arr: Array[Int]): Int = {
    @tailrec
    def loop(from: Int, to: Int): Int = {
      val mid = from + (to - from) / 2

      if (arr(mid) > arr(mid + 1) && arr(mid) > arr(mid - 1)) mid
      else if (arr(mid) > arr(mid + 1)) loop(from, mid - 1)
      else loop(mid + 1, to)
    }

    loop(0, arr.length - 1)
  }

  def averageOfLevels(root: TreeNode): Array[Double] = {
    @tailrec
    def loop(queue: Queue[List[TreeNode]], acc: Vector[Double]): Vector[Double] = {
      if (queue.isEmpty) acc
      else {
        val (listNodeLevel, remQueue) = queue.dequeue
        val newListLevel = listNodeLevel.foldLeft(List.empty[TreeNode]) { (list, node) =>
          if (node != null) addNodeToList(addNodeToList(list, node.left), node.right)
          else list
        }
        val newQueue = if (newListLevel.nonEmpty) remQueue.enqueue[List[TreeNode]](newListLevel) else remQueue
        val (levelSum, levelCount) = listNodeLevel.foldLeft((0L, 0)) { case ((sum, cont), curNode) =>
          (sum + curNode.value, cont + 1)
        }

        loop(newQueue, acc :+ (levelSum / levelCount.toDouble))
      }
    }

    def addNodeToList(list: List[TreeNode], node: TreeNode): List[TreeNode] = {
      if (node != null) node :: list else list
    }

    loop(Queue(List(root)), Vector.empty[Double]).toArray
  }

  def minDepth(root: TreeNode): Int = {
    @tailrec
    def loop(queue: Queue[TreeNode], minLevel: Int): Int = {
      val steps = queue.length
      val (newQueue, hasEnd) = bfs(queue, steps)

      if (!hasEnd) loop(newQueue, minLevel + 1)
      else minLevel
    }

    @tailrec
    def bfs(queue: Queue[TreeNode], steps: Int): (Queue[TreeNode], Boolean) = {
      if (steps == 0) (queue, false)
      else {
        val (node, newQueue) = queue.dequeue

        if (node.left != null || node.right != null) bfs(addNodeToQueue(addNodeToQueue(newQueue, node.left), node.right), steps - 1)
        else (newQueue, true)
      }
    }

    def addNodeToQueue(queue: Queue[TreeNode], node: TreeNode): Queue[TreeNode] = {
      if (node != null) queue.enqueue(node) else queue
    }

    if (root == null) 0
    else loop(Queue(root), 1)
  }

  def isSameTreeAnother(p: TreeNode, q: TreeNode): Boolean = {
    @tailrec
    def loop(pStack: List[TreeNode], qStack: List[TreeNode]): Boolean = {
      (pStack, qStack) match {
        case (Nil, Nil) => true
        case (Nil, _) | (_, Nil) => false
        case (pNext :: pRem, qNext :: qRem) =>
          if (isEqual(pNext, qNext)) loop(dfs(pNext, pRem), dfs(qNext, qRem))
          else false
      }
    }

    def isEqual(pNode: TreeNode, qNode: TreeNode): Boolean = {
      (Option(pNode), Option(qNode)) match {
        case (None, None) => true
        case (Some(pVal), Some(qVal)) => pVal.value == qVal.value
        case _ => false
      }
    }

    def dfs(node: TreeNode, stack: List[TreeNode]): List[TreeNode] =
      if (node != null) node.left :: node.right :: stack else stack

    loop(List(p), List(q))
  }

  def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {
    @tailrec
    def loop(nodeStack: List[TreeNode], visited: Set[TreeNode], curTarget: Int): Boolean = {
      nodeStack match {
        case curNode :: tail =>
          if (visited.contains(curNode)) loop(tail, visited, curTarget + curNode.value)
          else if (curNode.left == null && curNode.right == null) {
            if (curTarget - curNode.value == 0) true
            else loop(tail, visited, curTarget)
          }
          else {
            val newTarget = curTarget - curNode.value
            val newVisited = visited + curNode

            loop(addNodeToStack(addNodeToStack(nodeStack, curNode.left), curNode.right), newVisited, newTarget)
          }
        case Nil => false
      }
    }

    def addNodeToStack(stack: List[TreeNode], node: TreeNode): List[TreeNode] = {
      if (node != null) node :: stack else stack
    }

    if (root == null) false
    else loop(List(root), Set.empty[TreeNode], targetSum)
  }

  def maxDepth(root: TreeNode): Int = {
    @tailrec
    def loop(queue: Queue[TreeNode], maxDepth: Int): Int = {
      if (queue.isEmpty) maxDepth
      else {
        val steps = queue.length
        loop(bfs(queue, steps), maxDepth + 1)
      }
    }

    @tailrec
    def bfs(queue: Queue[TreeNode], steps: Int): Queue[TreeNode] = {
      if (steps == 0) queue
      else {
        val (node, newQueue) = queue.dequeue

        bfs(addNodeToQueue(addNodeToQueue(newQueue, node.left), node.right), steps - 1)
      }
    }

    def addNodeToQueue(queue: Queue[TreeNode], node: TreeNode): Queue[TreeNode] = {
      if (node != null) queue.enqueue(node) else queue
    }

    if (root == null) 0
    else loop(Queue(root), 1)
  }

  def diameterOfBinaryTree(root: TreeNode): Int = {
    def loop(node: TreeNode): (Int, Int) = {
      if (node == null) (0, 0)
      else {
        val (heightLeft, maxLeft) = loop(node.left)
        val (heightRight, maxRight) = loop(node.right)

        (1 + heightLeft.max(heightRight), maxLeft.max(maxRight).max(heightLeft + heightRight))
      }
    }

    loop(root)._2
  }

  def mergeTreesRec(root1: TreeNode, root2: TreeNode): TreeNode = {
    (Option(root1), Option(root2)) match {
      case (None, None) => null
      case (Some(r1), None) => r1
      case (None, Some(r2)) => r2
      case (Some(r1), Some(r2)) =>
        root1.value = r1.value + r2.value
        root1.left = mergeTrees(r1.left, r2.left)
        root1.right = mergeTrees(r1.right, r2.right)

        root1
    }
  }

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    @tailrec
    def loop(curNode: TreeNode): TreeNode = {
      if (p.value < curNode.value && q.value < curNode.value) loop(curNode.left)
      else if (p.value > curNode.value && q.value > curNode.value) loop(curNode.right)
      else curNode
    }

    loop(root)
  }

  def isSubtree(root: TreeNode, subRoot: TreeNode): Boolean = {
    @tailrec
    def loop(rootQueue: Queue[TreeNode]): Boolean = {
      if (rootQueue.isEmpty) false
      else {
        val (curNode, remQueue) = rootQueue.dequeue

        if (checkTrees(List(curNode), List(subRoot))) true
        else loop(bfs(curNode.left, bfs(curNode.right, remQueue)))
      }
    }

    @tailrec
    def checkTrees(pStack: List[TreeNode], qStack: List[TreeNode]): Boolean = {
      (pStack, qStack) match {
        case (Nil, Nil) => true
        case (Nil, _) | (_, Nil) => false
        case (pNext :: pRem, qNext :: qRem) =>
          if (isEqual(pNext, qNext)) checkTrees(dfs(pNext, pRem), dfs(qNext, qRem))
          else false
      }
    }

    def isEqual(pNode: TreeNode, qNode: TreeNode): Boolean = {
      (Option(pNode), Option(qNode)) match {
        case (None, None) => true
        case (Some(pVal), Some(qVal)) => pVal.value == qVal.value
        case _ => false
      }
    }

    def dfs(node: TreeNode, stack: List[TreeNode]): List[TreeNode] =
      if (node != null) node.left :: node.right :: stack else stack

    def bfs(node: TreeNode, queue: Queue[TreeNode]): Queue[TreeNode] =
      if (node != null) queue.enqueue(node) else queue

    loop(Queue(root))
  }

  def invertTree(root: TreeNode): TreeNode = {
    Option(root) match {
      case Some(node) =>
        val nodeRight = node.right
        node.right = node.left
        node.left = nodeRight

        invertTree(node.left)
        invertTree(node.right)

        root
      case None => null
    }
  }

  def twoSumDif(nums: Array[Int], target: Int): Array[Int] = {
    @tailrec
    def loop(curIndex: Int, map: Map[Int, Int]): Array[Int] = {
      val rem = target - nums(curIndex)

      if (map.contains(rem)) Array(map(rem), curIndex)
      else loop(curIndex + 1, map.updated(nums(curIndex), curIndex))
    }

    loop(0, Map.empty[Int, Int])
  }

  def sortedSquaresNew(nums: Array[Int]): Array[Int] = {
    @tailrec
    def loop(leftIndex: Int, rightIndex: Int, curFillIndex: Int, result: Array[Int]): Array[Int] = {
      if (curFillIndex < 0) result
      else {
        val left = nums(leftIndex).abs
        val right = nums(rightIndex).abs

        if (left > right) {
          result(curFillIndex) = Math.pow(left, 2).toInt
          loop(leftIndex + 1, rightIndex, curFillIndex - 1, result)
        } else {
          result(curFillIndex) = Math.pow(right, 2).toInt
          loop(leftIndex, rightIndex - 1, curFillIndex - 1, result)
        }
      }
    }

    val res = new Array[Int](nums.length)

    loop(0, nums.length - 1, nums.length - 1, res)
  }

  def backspaceCompare(s: String, t: String): Boolean = {
    @tailrec
    def loop(sIndex: Int, tIndex: Int): Boolean = {
      if (sIndex < 0 && tIndex < 0) true
      else if (sIndex < 0) {
        val (_, optTChar) = skip(t, tIndex, 0)

        optTChar match {
          case Some(_) => false
          case None => true
        }
      }
      else if (tIndex < 0) {
        val (_, optSChar) = skip(s, sIndex, 0)

        optSChar match {
          case Some(_) => false
          case None => true
        }
      }
      else {
        val (curSIndex, optSChar) = skip(s, sIndex, 0)
        val (curTIndex, optTChar) = skip(t, tIndex, 0)

        (optSChar, optTChar) match {
          case (None, None) => true
          case (None, _) | (_, None) => false
          case (Some(sChar), Some(tChar)) =>
            if (sChar == tChar) loop(curSIndex - 1, curTIndex - 1)
            else false
        }
      }
    }

    def skip(str: String, index: Int, curBackspaces: Int): (Int, Option[Char]) = {
      if (index < 0) (index, None)
      else {
        val curChar = str(index)

        if (curChar == '#') skip(str, index - 1, curBackspaces + 1)
        else if (curBackspaces > 0) skip(str, index - 1, curBackspaces - 1)
        else (index, Some(curChar))
      }
    }

    loop(s.length - 1, t.length - 1)
  }

  def majorityElement(nums: Array[Int]): Int = {
    @tailrec
    def loop(curIndex: Int, curPopulation: Int, result: Int): Int = {
      if (curIndex == nums.length) result
      else if (curPopulation == 0) loop(curIndex + 1, curPopulation + 1, nums(curIndex))
      else if (result == nums(curIndex)) loop(curIndex + 1, curPopulation + 1, result)
      else loop(curIndex + 1, curPopulation - 1, result)
    }

    loop(0, 0, -1)
  }

  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val result = Array.fill(nums.length)(1)

    (1 until nums.length).foldLeft(1) { (multiplier, nextIndex) =>
      result(nextIndex) = nums(nextIndex - 1) * multiplier

      result(nextIndex)
    }

    ((nums.length - 2) to 0 by -1).foldLeft(nums(nums.length - 1)) { (multiplier, nextIndex) =>
      result(nextIndex) = result(nextIndex) * multiplier

      nums(nextIndex) * multiplier
    }

    result
  }

  def findDuplicate(nums: Array[Int]): Int = {
    @tailrec
    def loop(slow: Int, fast: Int): Int = {
      if (slow == fast) getCycleStart(nums(0), fast)
      else {
        loop(nums(slow), nums(nums(fast)))
      }
    }

    @tailrec
    def getCycleStart(slow: Int, fast: Int): Int = {
      if (slow == fast) slow
      else getCycleStart(nums(slow), nums(fast))
    }

    loop(nums(nums(0)), nums(nums(nums(0))))
  }

  def construct2DArray(original: Array[Int], m: Int, n: Int): Array[Array[Int]] = {
    @tailrec
    def loop(row: Int, column: Int, res: Array[Array[Int]]): Array[Array[Int]] = {
      if (row == m) res
      else if (column == n) loop(row + 1, 0, res)
      else {
        res(row)(column) = original(original.length / m * row + column)

        loop(row, column + 1, res)
      }
    }

    if (m * n == original.length) {
      loop(0, 0, Array.fill(m)(new Array[Int](n)))
    }
    else Array.empty[Array[Int]]
  }

  def findDuplicates(nums: Array[Int]): List[Int] = {
    @tailrec
    def loop(curIndex: Int): List[Int] = {
      if (curIndex == nums.length) getDuplicates(0, List.empty[Int])
      else {
        val curNum = nums(curIndex)

        if (nums(curNum - 1) != curNum) {
          nums(curIndex) = nums(curNum - 1)
          nums(curNum - 1) = curNum

          loop(curIndex)
        }
        else loop(curIndex + 1)
      }
    }

    @tailrec
    def getDuplicates(curIndex: Int, acc: List[Int]): List[Int] = {
      if (curIndex == nums.length) acc.reverse
      else if (nums(curIndex) != curIndex + 1) getDuplicates(curIndex + 1, (curIndex + 1) :: acc)
      else getDuplicates(curIndex + 1, acc)
    }

    loop(0)
  }

  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    @tailrec
    def loop(row: Int, column: Int, needClearFirstRow: Boolean, needClearFirstColumn: Boolean): Unit = {
      if (row == matrix.length) fillZeroes(needClearFirstRow, needClearFirstColumn)
      else if (column == matrix(0).length) loop(row + 1, 0, needClearFirstRow, needClearFirstColumn)
      else if (matrix(row)(column) == 0) {
        if (row == 0 && column == 0) loop(row, column + 1, true, true)
        else if (row == 0) loop(row, column + 1, true, needClearFirstColumn)
        else if (column == 0) loop(row, column + 1, needClearFirstRow, true)
        else {
          if (matrix(row)(0) != 0) matrix(row)(0) = 0
          if (matrix(0)(column) != 0) matrix(0)(column) = 0

          loop(row, column + 1, needClearFirstRow, needClearFirstColumn)
        }
      }
      else loop(row, column + 1, needClearFirstRow, needClearFirstColumn)
    }

    def fillZeroes(needClearFirstRow: Boolean, needClearFirstColumn: Boolean): Unit = {
      (1 until matrix.length).foreach(row => if (matrix(row)(0) == 0) setZeroForRow(row, 0))
      (1 until matrix(0).length).foreach(column => if (matrix(0)(column) == 0) setZeroForColumn(0, column))

      if (needClearFirstRow) setZeroForRow(0, 0)
      if (needClearFirstColumn) setZeroForColumn(0, 0)
    }

    @tailrec
    def setZeroForColumn(row: Int, column: Int): Unit = {
      if (row == matrix.length) ()
      else {
        matrix(row)(column) = 0

        setZeroForColumn(row + 1, column)
      }
    }

    @tailrec
    def setZeroForRow(row: Int, column: Int): Unit = {
      if (column == matrix(0).length) ()
      else {
        matrix(row)(column) = 0

        setZeroForRow(row, column + 1)
      }
    }

    loop(0, 0, false, false)
  }

  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    @tailrec
    def loop(rowFrom: Int, rowTo: Int, columnFrom: Int, columnTo: Int, list: List[Int]): List[Int] = {
      if (rowFrom > rowTo || columnFrom > columnTo) list.reverse
      else if (rowFrom == rowTo) {
        (columnFrom to columnTo).foldLeft(list)((acc, columnIndex) => matrix(rowFrom)(columnIndex) :: acc).reverse
      }
      else if (columnFrom == columnTo) {
        (rowFrom to rowTo).foldLeft(list)((acc, rowIndex) => matrix(rowIndex)(columnFrom) :: acc).reverse
      }
      else {
        val firstStepList = (columnFrom to columnTo).foldLeft(list)((acc, columnIndex) => matrix(rowFrom)(columnIndex) :: acc)
        val secondStepList = (rowFrom + 1 to rowTo).foldLeft(firstStepList)((acc, rowIndex) => matrix(rowIndex)(columnTo) :: acc)
        val thirdStepList = (columnTo - 1 to columnFrom by -1).foldLeft(secondStepList)((acc, columnIndex) => matrix(rowTo)(columnIndex) :: acc)
        val fourthStepList = (rowTo - 1 until rowFrom by -1).foldLeft(thirdStepList)((acc, rowIndex) => matrix(rowIndex)(columnFrom) :: acc)

        loop(rowFrom + 1, rowTo - 1, columnFrom + 1, columnTo - 1, fourthStepList)
      }
    }

    loop(0, matrix.length - 1, 0, matrix(0).length - 1, List.empty[Int])
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    @tailrec
    def loop(l1Node: Option[ListNode], l2Node: Option[ListNode], rem: Int, res: ListNode): Unit = {
      if (l1Node.isDefined || l2Node.isDefined || rem > 0) {
        val l1Num = l1Node.map(_.x).getOrElse(0)
        val l2Num = l2Node.map(_.x).getOrElse(0)
        val sum = l1Num + l2Num + rem
        val newRem = sum % 10

        res.next = new ListNode(sum / 10, null)

        loop(l1Node.flatMap(node => Option(node.next)), l2Node.flatMap(node => Option(node.next)), newRem, res.next)
      }
    }

    val res = new ListNode(0, null)

    loop(Option(l1), Option(l2), 0, res)

    res.next
  }

  /*
      def genNumber(node: ListNode, counter: Int, acc: Long): Long = {
        Option(node) match {
          case Some(value) =>
            genNumber(value.next, counter + 1, value.x * Math.pow(10, counter).toLong + acc)
          case None => acc
        }
      }

      def genResNode(curNum: Long, res: ListNode): Unit = {
        if (curNum < 10) res.next = new ListNode(curNum.toInt, null)
        else {
          res.next = new ListNode((curNum % 10).toInt, null)
          genResNode(curNum / 10, res.next)
        }
      }

      val l1Num = genNumber(l1, 0, 0)
      val l2Num = genNumber(l2, 0, 0)
      val res = new ListNode(0, null)

      genResNode(l1Num + l2Num, res)

      res.next
    }
   */

  def numIslands(grid: Array[Array[Char]]): Int = {
    def dfs(n: Int, m: Int): Unit = {
      if (n >= 0 && m >= 0 && n < grid.length && m < grid(0).length) {
        if (grid(n)(m) == '1') {
          grid(n)(m) = '0'

          dfs(n - 1, m)
          dfs(n + 1, m)
          dfs(n, m - 1)
          dfs(n, m + 1)
        }
      }
    }

    grid.indices.foldLeft(0) { (acc, n) => grid(0).indices.foldLeft(acc) {(res, m) =>
      if (grid(n)(m) == '1') {
        dfs(n, m)

        res + 1
      } else res
    }}
  }

  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    @tailrec
    def loop(currIndex: Int, accArray: Array[Int], result: ArrayBuffer[Array[Int]]): Array[Array[Int]] = {
      if (currIndex == intervals.length) result.append(accArray).toArray
      else {
        val curArray = intervals(currIndex)

        if (accArray(1) >= curArray(0)) {
          accArray(1) = accArray(1).max(curArray(1))
          loop(currIndex + 1, accArray, result)
        }
        else loop(currIndex + 1, curArray, result += accArray)
      }
    }

    intervals.sortInPlaceBy(_(0))
    loop(1, intervals(0), ArrayBuffer.empty[Array[Int]])
  }


  /*
  firstList =
  [[0,2],[5,10],[13,23],[24,25]]
  secondList =
  [[1,5],[8,12],[15,24],[25,26]]
   */
  def intervalIntersection(firstList: Array[Array[Int]], secondList: Array[Array[Int]]): Array[Array[Int]] = {
    @tailrec
    def loop(firstIndex: Int, secondIndex: Int, buf: ArrayBuffer[Array[Int]]): Array[Array[Int]] = {
      if (firstIndex == firstList.length || secondIndex == secondList.length) buf.toArray
      else {
        val left = firstList(firstIndex)(0).max(secondList(secondIndex)(0))
        val right = firstList(firstIndex)(1).min(secondList(secondIndex)(1))

        if (left <= right)
          buf += Array(left, right)

        if (firstList(firstIndex)(1) > secondList(secondIndex)(1)) loop(firstIndex + 1, secondIndex, buf)
        else loop(firstIndex, secondIndex + 1, buf)
      }

    }

    loop(0, 0, ArrayBuffer.empty[Array[Int]])
  }

  def isValidBST(root: TreeNode): Boolean = {
    def loop(node: TreeNode, minOpt: Option[Int], maxOpt: Option[Int]): Boolean = {
      if (node == null) true
      else {
        val isWrong =
          (minOpt, maxOpt) match {
            case (Some(min), Some(max)) =>
              node.value <= min || node.value >= max
            case (Some(min), None) =>
              node.value <= min
            case (None, Some(max)) =>
              node.value >= max
            case _ => false
          }

        if (isWrong) false
        else loop(node.left, minOpt, Some(node.value)) && loop(node.right, Some(node.value), maxOpt)
      }
    }

    loop(root, None, None)
  }

  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    @tailrec
    def loop(nodes: Queue[TreeNode], acc: ListBuffer[List[Int]], isReversed: Boolean): List[List[Int]] = {
      if (nodes.isEmpty) acc.toList
      else {
        val levelSize = nodes.length
        val (newQueue, list) = fillList(nodes, levelSize, ListBuffer.empty[Int])

        loop(newQueue, acc += (if (isReversed) list.reverse else list), !isReversed)
      }
    }

    @tailrec
    def fillList(nodes: Queue[TreeNode], remLevelSize: Int, acc: ListBuffer[Int]): (Queue[TreeNode], List[Int]) = {
      if (remLevelSize == 0) (nodes, acc.toList)
      else {
        val (curNode, remQueue) = nodes.dequeue
        acc += curNode.value

        fillList(bfs(curNode.left, bfs(curNode.right, remQueue)), remLevelSize - 1, acc)
      }
    }

    def bfs(node: TreeNode, queue: Queue[TreeNode]): Queue[TreeNode] =
      if (node != null) queue.enqueue(node) else queue

    if (root == null) List.empty[List[Int]]
    else loop(Queue(root), ListBuffer.empty[List[Int]], true)
  }

  def generateParenthesis(n: Int): List[String] = {
    def loop(left: Int, right: Int, acc: ListBuffer[String], curString: String): Unit = {
      if (curString.length == n * 2) acc += curString

      if (left < n) {
        loop(left + 1, right, acc, curString + "(")
      }
      if (right < left) {
        loop(left, right + 1, acc, curString + ")")
      }
    }

    val res = ListBuffer.empty[String]
    loop(0, 0, res, "")

    res.toList
  }

  def longestPalindromeStr(s: String): String = {
    @tailrec
    def loop(left: Int, right: Int): (Int, Int) = {
      if (left < 0 || right == s.length || s(left) != s(right)) (left + 1, right - 1)
      else loop(left - 1, right + 1)
    }

    def getMax(odd: (Int, Int), even: (Int, Int), max: String): String = {
      val oddLength = odd._2 - odd._1 + 1
      val evenLength = even._2 - even._1 + 1
      val (curMax, curMaxLength) = if (oddLength > evenLength) (odd, oddLength) else (even, evenLength)

      if (curMaxLength > max.length) s.substring(curMax._1, curMax._2 + 1) else max
    }

    if (s.length <= 1) s
    else {
      s.indices.foldLeft("") { (res, index) =>
        val curMaxOdd = loop(index, index)
        val curMaxEven = if (index == s.length - 1) curMaxOdd else loop(index, index + 1)

        getMax(curMaxOdd, curMaxEven, res)
      }
    }
  }

  def characterReplacement(s: String, k: Int): Int = {
    @tailrec
    def loop(start: Int, curIndex: Int, remReplacements: Int, curMax: Int, genMax: Int): Int = {
      if (start == s.length) genMax
      else if (curIndex == s.length) (curMax + remReplacements.min(start)).max(genMax)
      else if (s(start) == s(curIndex)) loop(start, curIndex + 1, remReplacements, curMax + 1, genMax)
      else {
        if (remReplacements > 0) loop(start, curIndex + 1, remReplacements - 1, curMax + 1, genMax)
        else loop(start + 1, start + 1, k, 0, curMax.max(genMax))
      }
    }

    loop(0, 0, k, 0, 0)
  }

  def characterReplacementArray(s: String, k: Int): Int = {
    @tailrec
    def loop(left: Int, right: Int, counter: Array[Int], max: Int): Int = {
      if (right == s.length) max
      else {
        counter(s(right) - 'A') += 1
        if ((right - left + 1) - counter.max <= k) loop(left, right + 1, counter, max.max(right - left + 1))
        else {
          counter(s(left) - 'A') -= 1
          loop(left + 1, right + 1, counter, max)
        }
      }
    }

    loop(0, 0, Array.fill(26)(0), 0)
  }

  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    @tailrec
    def loop(arrayListIndex: Int, resIndex: Int, array: Array[List[Int]], curList: List[Int], res: Array[Int]): Array[Int] = {
      if (resIndex == res.length) res
      else {
        curList match {
          case head :: tail =>
            res(resIndex) = head
            loop(arrayListIndex, resIndex + 1, array, tail, res)
          case Nil =>
            loop(arrayListIndex - 1, resIndex, array, array(arrayListIndex), res)
        }
      }
    }

    val counter = nums.foldLeft(Map.empty[Int, Int]) {(map, next) =>
      map.updated(next, map.getOrElse(next, 0) + 1)
    }.groupMap(_._2)(_._1).view.mapValues(_.toList).toMap
    val arrayOfList = new Array[List[Int]](nums.length)
    val arrayResult = new Array[Int](k)

    arrayOfList.indices.foreach(index => arrayOfList(index) = counter.getOrElse(index + 1, List.empty))

    loop(arrayOfList.length - 1, 0, arrayOfList, arrayOfList(arrayOfList.length - 1), arrayResult)
  }

  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {
    @tailrec
    def loop(left: Int, right: Int): List[Int] = {
      if (left >= right) arr.slice(left, left + k).toList
      else {
        val mid = left + (right - left) / 2

        if (x - arr(mid) > arr(mid + k) - x)
          loop(mid + 1, right)
        else
          loop(left, mid - 1)
      }
    }

    loop(0, arr.length - k)
  }

  def findMin(nums: Array[Int]): Int = {
    @tailrec
    def loop(left: Int, right: Int, res: Int): Int = {
      if (left > right) res
      else if (nums(left) < nums(right)) res.min(nums(left))
      else{
        val mid = left + (right - left) / 2

        if (nums(mid) >= nums(left)) loop(mid + 1, right, res.min(nums(mid)))
        else loop(left, mid - 1, res.min(nums(mid)))
      }
    }

    loop(0, nums.length - 1, nums(0))
  }

  def searchBinSearch(nums: Array[Int], target: Int): Int = {
    @tailrec
    def loop(left: Int, right: Int): Int = {
      if (left > right) -1
      else {
        val mid = left + (right - left) / 2

        if (nums(mid) == target) mid
        else if (nums(left) <= nums(mid)) {
          if (nums(left) > target || nums(mid) < target) loop(mid + 1, right)
          else loop(left, mid - 1)
        }
        else {
          if (nums(right) < target || nums(mid) > target) loop(left, mid - 1)
          else loop(mid + 1, right)
        }
      }
    }

    loop(0, nums.length - 1)
  }
}