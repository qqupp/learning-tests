package learning.minijson

import learning.minijson.Zipper.{Cursor, Result}

object zzzMoveUtils {

  // old interface to move
  sealed trait Move
  case object U extends Move
  case object D extends Move
  case object R extends Move
  case object L extends Move

  def moveTo(cursor: Cursor, moves: Move *): Result[Cursor] = moves.toList match {
    case Nil => Right(cursor)
    case U :: ms =>  cursor.goUp.flatMap(moveTo(_, ms :_*))
    case D :: ms =>  cursor.goDown.flatMap(moveTo(_, ms :_*))
    case L :: ms =>  cursor.goLeft.flatMap(moveTo(_, ms :_*))
    case R :: ms =>  cursor.goRight.flatMap(moveTo(_, ms :_*))
  }

}
