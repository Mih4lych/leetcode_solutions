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

  def test_th_2h3h4h5hTs(): Unit = {
    assertEquals(
      "Ac6c=As6s AhQs",
      process("texas-holdem 2h3h4h5hTs AhQs As6s Ac6c"),
    )
  }

  def test_th_2h2s3h3s5d(): Unit = {
    assertEquals(
      "Kc6d Ac7d",
      process("texas-holdem 2h2s3h3s5d Ac7d Kc6d"),
    )
  }

  def test_th_2h2s3h3sKd(): Unit = {
    assertEquals(
      "Qc7d Kc6d",
      process("texas-holdem 2h2s3h3sKd Qc7d Kc6d"),
    )
  }
}
