package leetcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LeetCode extends App{
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
    nums.foldLeft((0, 0)){ (pair, nextNum) =>
      val (index, uniqNums) = pair

      if (uniqNums == 0 && index == 0) {
        (0, 1)
      }
      else {
        if (nums(index) == nextNum) {
          (index, uniqNums)
        }
        else {
          nums(index + 1) = nextNum
          (index + 1, uniqNums + 1)
        }
      }
    }._2
  }
}
