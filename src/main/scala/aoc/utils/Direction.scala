package aoc.utils

sealed trait Direction {
  def opposite: Direction

  def getAscii: Char

  def getPerpendiculars: Seq[Direction]
}

sealed trait Horizontal extends Direction {

  override def getPerpendiculars: Seq[Direction] = Seq(Direction.Up, Direction.Down)
}

sealed trait Vertical extends Direction {

  override def getPerpendiculars: Seq[Direction] = Seq(Direction.Left, Direction.Right)
}

object Direction {

  case object Up extends Vertical {
    override def opposite: Direction = Down

    override def getAscii: Char = '^'
  }

  case object Down extends Vertical {
    override def opposite: Direction = Up

    override def getAscii: Char = 'v'
  }

  case object Left extends Horizontal {
    override def opposite: Direction = Right

    override def getAscii: Char = '<'
  }

  case object Right extends Horizontal {
    override def opposite: Direction = Left

    override def getAscii: Char = '>'
  }
}
