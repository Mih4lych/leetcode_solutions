package testtasks.packer

import junit.framework.TestCase
import org.junit.Assert._
import testtasks.pocker.Solver.process

class TexasHoldemSolverTest extends TestCase {
  def test_th_5c6dAcAsQs(): Unit = {
    assertEquals(
      "2cJc Kh4h=Ks4c Kc7h KdJs 6h7d 2hAh",
      process("texas-holdem 5c6dAcAsQs Ks4c KdJs 2hAh Kh4h Kc7h 6h7d 2cJc"),
    )
  }

  def test_th_2h5c8sAsKc(): Unit = {
    assertEquals(
      "Jc6s Qs9h 3cKh KdQh",
      process("texas-holdem 2h5c8sAsKc Qs9h KdQh 3cKh Jc6s"),
    )
  }

  def test_th_3d4s5dJsQd(): Unit = {
    assertEquals(
      "9h7h 2dTc KcAs 7sJd TsJc Qh8c 5c4h",
      process("texas-holdem 3d4s5dJsQd 5c4h 7sJd KcAs 9h7h 2dTc Qh8c TsJc"),
    )
  }
}
