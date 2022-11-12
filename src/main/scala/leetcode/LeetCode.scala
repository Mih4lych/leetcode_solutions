package leetcode

import scala.annotation.tailrec
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
}
