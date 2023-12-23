package aoc.utils

sealed trait Direction {

  def opposite: Direction

  def restOfDirs: List[Direction]

  def getAscii: Char

  def getPerpendiculars: Seq[Direction]

  def adjustCoordinates(x: Int, y: Int): (Int, Int)
}

sealed trait Horizontal extends Direction {

  override def getPerpendiculars: Seq[Direction] = Seq(Direction.Up, Direction.Down)
}

sealed trait Vertical extends Direction {

  override def getPerpendiculars: Seq[Direction] = Seq(Direction.Left, Direction.Right)
}

object Direction {

  def parse(c: Char): Direction = c.toLower match {
    case 'r' => Right
    case 'l' => Left
    case 'u' => Up
    case 'd' => Down
  }

  def parse(s: String): Direction = parse(s.charAt(0))

  case object Up extends Vertical {
    override def opposite: Direction = Down

    override def getAscii: Char = '^'

    override def restOfDirs: List[Direction] = List(Down, Left, Right)

    override def adjustCoordinates(x: Int, y: Int): (Int, Int) = (x, y - 1)
  }

  case object Down extends Vertical {
    override def opposite: Direction = Up

    override def getAscii: Char = 'v'

    override def restOfDirs: List[Direction] = List(Up, Left, Right)

    override def adjustCoordinates(x: Int, y: Int): (Int, Int) = (x, y + 1)
  }

  case object Left extends Horizontal {
    override def opposite: Direction = Right

    override def getAscii: Char = '<'

    override def restOfDirs: List[Direction] = List(Down, Up, Right)

    override def adjustCoordinates(x: Int, y: Int): (Int, Int) = (x - 1, y)
  }

  case object Right extends Horizontal {
    override def opposite: Direction = Left

    override def getAscii: Char = '>'

    override def restOfDirs: List[Direction] = List(Down, Left, Up)

    override def adjustCoordinates(x: Int, y: Int): (Int, Int) = (x + 1, y)
  }
}
