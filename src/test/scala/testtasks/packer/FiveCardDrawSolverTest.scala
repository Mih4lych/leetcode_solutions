package testtasks.packer

import junit.framework.TestCase
import org.junit.Assert._
import testtasks.pocker.Solver.process

class FiveCardDrawSolverTest extends TestCase {
  def test_5cd_4s5hTsQh9h(): Unit = {
    assertEquals(
      "4s5hTsQh9h Qc8d7cTcJd 5s5d7s4dQd 7h6h7d2cJc 3cKs4cKdJs 2hAhKh4hKc As6d5cQsAc",
      process("five-card-draw 4s5hTsQh9h Qc8d7cTcJd 5s5d7s4dQd 3cKs4cKdJs 2hAhKh4hKc 7h6h7d2cJc As6d5cQsAc"),
    )
  }

  def test_5cd_7h4s4h8c9h(): Unit = {
    assertEquals(
      "4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c",
      process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c"),
    )
  }

  def test_5cd_5s3s4c2h9d(): Unit = {
    assertEquals(
      "5s3s4c2h9d 4h6s8hJd5d 5c3cQdTd9s 8dKsTc6c2c 8c3d7h7dTs KhJs9c5h9h AhQhKcQc2d",
      process("five-card-draw 5s3s4c2h9d 8dKsTc6c2c 4h6s8hJd5d 5c3cQdTd9s AhQhKcQc2d KhJs9c5h9h 8c3d7h7dTs"),
    )
  }

  def test_5cd_2h3h4h5hAh(): Unit = {
    assertEquals(
      "2h3h4h5hAh AcKcQcJcTc",
      process("five-card-draw 2h3h4h5hAh AcKcQcJcTc"),
    )
  }

  def test_5cd_2c2s3c3sKdQh(): Unit = {
    assertEquals(
      "2h2d3h3dKsQc=2c2s3c3sKdQh",
      process("five-card-draw 2c2s3c3sKdQh 2h2d3h3dKsQc"),
    )
  }

  def test_5cd_2c2s3c3sKdJh(): Unit = {
    assertEquals(
      "2c2s3c3sKdJh 2h2d3h3dKsQc",
      process("five-card-draw 2c2s3c3sKdJh 2h2d3h3dKsQc"),
    )
  }
}
