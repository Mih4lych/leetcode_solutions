package leetcode

import org.scalatest.funsuite.AnyFunSuite

class TestLeetCode extends AnyFunSuite {
  test("Test minWindow") {
    assert(LeetCode.minWindow("a", "aa") == "")
    assert(LeetCode.minWindow("ADOBECODEBANC", "ABC") == "BANC")
    assert(LeetCode.minWindow("a", "a") == "a")

    println("pass minWindow test")
  }

  test("Test maxArea") {
    assert(LeetCode.maxArea(Array(1, 1)) == 1)
    assert(LeetCode.maxArea(Array(1, 8, 6, 2, 5, 4, 8, 3, 7)) == 49)

    println("pass maxArea test")
  }

  test("Test findErrorNums") {
    assert(LeetCode.findErrorNums(Array(1, 1)).sameElements(Array(1, 2)))
    assert(LeetCode.findErrorNums(Array(1, 2, 3, 4, 5, 5, 7, 8, 9)).sameElements(Array(5, 6)))
    assert(LeetCode.findErrorNums(Array(1)).sameElements(Array(1)))
    assert(LeetCode.findErrorNums(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 11)).sameElements(Array(12, 11)))

    println("pass findErrorNums test")
  }

  test("Test romanToInt") {
    assert(LeetCode.romanToInt("III") == 3)
    assert(LeetCode.romanToInt("LVIII") == 58)
    assert(LeetCode.romanToInt("MCMXCIV") == 1994)

    println("pass romanToInt test")
  }

  test("Test recSolution") {
    assert(LeetCode.checkSubarraySum(Array(23,2,4,6,7), 6))
    assert(LeetCode.checkSubarraySum(Array(23,2,6,4,7), 6))
    assert(!LeetCode.checkSubarraySum(Array(23,2,6,4,7), 13))

    println("pass recSolution test")
  }

  test("Test minMutation") {
    assert(LeetCode.minMutation("AACCTTGG", "AATTCCGG", Array("AATTCCGG","AACCTGGG","AACCCCGG","AACCTACC")) == -1)

    println("pass minMutation test")
  }

  test("Test removeDuplicates") {
    val testArray = Array(1,1,2)
    assert(LeetCode.removeDuplicates(testArray) == 2)
    assert(testArray.sameElements(Array(1, 2, 2)))

    println("pass removeDuplicates test")
  }
}