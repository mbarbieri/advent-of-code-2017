package advent

object Day03 extends App {

  sealed trait Direction {
    val directions = List(Right, Up, Left, Down, Right)

    def next: Direction = {
      directions(directions.indexOf(this) + 1)
    }
  }

  object Right extends Direction

  object Up extends Direction

  object Left extends Direction

  object Down extends Direction

  def movements(): Stream[Direction] = {

    def recur(current: Direction, n: Int): Stream[Direction] = {
      (Stream.fill(n)(current) #::: Stream.fill(n)(current.next)) #::: recur(current.next.next, n + 1)
    }

    recur(Right, 1)
  }

  case class Square(r: Int, c: Int) {
    def move(direction: Direction): Square = direction match {
      case Right => Square(r, c + 1)
      case Up => Square(r + 1, c)
      case Left => Square(r, c - 1)
      case Down => Square(r - 1, c)
    }
  }

  def squareOfValue(x: Int): Square = {
    movements().take(x - 1).foldLeft(Square(0, 0))((square, direction) => square.move(direction))
  }

  def distance(square: Square): Int = Math.abs(square.r) + Math.abs(square.c)

  def solve(input: Int): Int = distance(squareOfValue(input))

  println(solve(265149))

}
