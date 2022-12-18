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

  test("Test removeStones") {
    assert(LeetCode.removeStones(Array(Array(0,0), Array(0,1), Array(1,0), Array(1,2), Array(2,1), Array(2, 2))) == 5)
    assert(LeetCode.removeStones(Array(Array(0,0))) == 0)

    println("pass removeStones test")
  }

  test("Test countNodes") {
    val root = LeetCode.TreeNode(1)
    root.left = LeetCode.TreeNode(2)
    root.right = LeetCode.TreeNode(3)

    val rootLeft = root.left
    rootLeft.left = LeetCode.TreeNode(4)
    rootLeft.right = LeetCode.TreeNode(5)

    val rootRight = root.right
    rootRight.left = LeetCode.TreeNode(6)

    assert(LeetCode.countNodes(root) == 6)

    println("pass countNodes test")
  }

  test("Test computeArea") {
    assert(LeetCode.computeArea(ax1 = -3, ay1 = 0, ax2 = 3, ay2 = 4, bx1 = 0, by1 = -1, bx2 = 9, by2 = 2) == 45)

    println("pass computeArea test")
  }

  test("Test isUgly") {
    assert(!LeetCode.isUgly(-2147483648))

    println("pass isUgly test")
  }

  test("Test search") {
    assert(LeetCode.search(Array(-1,0,3,5,9,12), 2) == -1)

    println("pass search test")
  }

  test("Test searchInsertByHands") {
    assert(LeetCode.searchInsertByHands(Array(1,3), 2) == 1)

    println("pass searchInsertByHands test")
  }

  test("Test nearestExit") {
    assert(LeetCode.nearestExit(Array(Array('+','+','.','+'), Array('.','.','.','+'), Array('+','+','+','.')), Array(1,2)) == 1)
    assert(LeetCode.nearestExit(Array(Array('+','+','+'), Array('.','.','.'), Array('+','+','+')), Array(1,0)) == 2)
    assert(LeetCode.nearestExit(Array(Array('.','+')), Array(0,0)) == -1)
    assert(LeetCode.nearestExit(Array(
      Array('+','.','+','+','+','+','+'),
      Array('+','.','+','.','.','.','+'),
      Array('+','.','+','.','+','.','+'),
      Array('+','.','.','.','+','.','+'),
      Array('+','+','+','+','+','.','+')
    ), Array(0,1)) == 12)

    println("pass nearestExit test")
  }

  test("Test twoSum") {
    assert(LeetCode.twoSum(Array(5,25,75), 100) == Array(2,3))

    println("pass twoSum test")
  }

  test("Test numSquares") {
    assert(LeetCode.numSquares(12) == 3)

    println("pass numSquares test")
  }

  test("Test isValidSudoku") {
    assert(LeetCode.isValidSudoku(Array(Array('5','3','.','.','7','.','.','.','.'),Array('6','.','.','1','9','5','.','.','.'),Array('.','9','8','.','.','.','.','6','.'),Array('8','.','.','.','6','.','.','.','3'),Array('4','.','.','8','.','3','.','.','1'),Array('7','.','.','.','2','.','.','.','6'),Array('.','6','.','.','.','.','2','8','.'),Array('.','.','.','4','1','9','.','.','5'),Array('.','.','.','.','8','.','.','7','9'))))

    println("pass isValidSudoku test")
  }

  test("Test checkInclusion") {
    assert(LeetCode.checkInclusion("adc", "dcda"))

    println("pass checkInclusion test")
  }

  test("Test floodFill") {
    assert(LeetCode.floodFill(Array(Array(1,1,1),Array(1,1,0),Array(1,0,1)), 1, 1, 2).sameElements(Array(Array(2,2,2),Array(2,2,0),Array(2,0,1))))

    println("pass floodFill test")
  }

  test("Test maxAreaOfIsland") {
    assert(LeetCode.maxAreaOfIsland(Array(Array(1,1,0,0,0),Array(1,1,0,0,0),Array(0,0,0,1,1),Array(0,0,0,1,1))) == 4)

    println("pass maxAreaOfIsland test")
  }

  test("Test halvesAreAlike") {
    assert(LeetCode.halvesAreAlike("book"))

    println("pass halvesAreAlike test")
  }

  test("Test minimumAverageDifference") {
    assert(LeetCode.minimumAverageDifference(Array(2,5,3,9,5,3)) == 3)
    assert(LeetCode.minimumAverageDifference(Array(0)) == 0)
    assert(LeetCode.altMinAvg(Array(2,5,3,9,5,3)) == 3)
    assert(LeetCode.altMinAvg(Array(0)) == 0)
    println("pass minimumAverageDifference test")
  }

  test("Test evalRPN") {
    assert(LeetCode.evalRPN(Array("2","1","+","3","*")) == 9)
    assert(LeetCode.evalRPN(Array("10","6","9","3","+","-11","*","/","*","17","+","5","+")) == 22)
    println("pass evalRPN test")
  }

  test("Test dailyTemperatures") {
    assert(LeetCode.dailyTemperatures(Array(73,74,75,71,69,72,76,73)).sameElements(Array(1,1,4,2,1,1,0,0)))
    println("pass dailyTemperatures test")
  }
}
