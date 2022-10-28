import org.scalatest.funsuite.AnyFunSuite

class TestTests extends AnyFunSuite{
  test("test permutations") {
    assert(Tests.permutations("ABC").equals(Seq("ABC", "ACB", "BAC", "BCA", "CAB", "CBA")))
    assert(Tests.permutationsByHands("ABC").equals(Seq("ABC", "ACB", "BAC", "BCA", "CAB", "CBA")))
  }
}
