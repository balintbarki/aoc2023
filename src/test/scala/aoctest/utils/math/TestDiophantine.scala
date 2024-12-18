package aoctest.utils.math

import aoc.utils.math.DiophantineEquation
import org.junit.{Assert, Test}

class TestDiophantine {

  @Test
  def testHasSolution(): Unit = {
    Seq(
      ((94, 22, 8400), true),
      ((34, 67, 5400), true),
      ((69, 23, 18641), false),
      ((23, 71, 10279), true),
      ((66, 21, 12176), false),
    ).foreach { case ((a, b, c), expectedHasSolution) =>
      val actualHasSolution = new DiophantineEquation(a, b, c).hasSolution
      Assert.assertEquals(s"Diophantin equation ${a}x + ${b}y = $c hasSolution incorrect.", expectedHasSolution,
        actualHasSolution
      )
    }
  }

  @Test
  def testAllSolutions(): Unit = {
    Seq(
      ((11, 67, -56, 0, 1, -1, 0), Seq((1, -1))),
      ((94, 22, 8400, 0, 100, 0, 100), Seq((69, 87), (80, 40))),
      ((34, 67, 5400, 0, 100, 0, 100), Seq((13, 74), (80, 40))),
    ).foreach { case ((a, b, c, minx, maxx, miny, maxy), expectedSolutions) =>
      val actualSolutions = new DiophantineEquation(a, b, c).allSolutions(minx, maxx, miny, maxy)
      Assert.assertEquals(expectedSolutions, actualSolutions)
    }
  }
}
