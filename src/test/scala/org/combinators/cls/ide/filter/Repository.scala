package org.combinators.cls.ide.filter

import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

class Repository(val labyrinth: Array[Array[Boolean]], val start: (Int, Int), val goal: (Int, Int)) {

  def intToType(x: Int): Type = {
    Constructor(x.toString)
  }

  def position(pos: (Int, Int)): Type = {
    'Pos (intToType(pos._1), intToType(pos._2))
  }

  def possibleMoves(deltaX: Int, deltaY: Int): Seq[Type] = {
    for (y <- labyrinth.indices;
         x <- labyrinth(y).indices;
         nextX = x + deltaX;
         nextY = y + deltaY
         if (labyrinth.indices.contains(nextY) &&
           labyrinth(nextY).indices.contains(nextX) &&
           labyrinth(nextY)(nextX) &&
           labyrinth(y)(x)))
      yield position((x, y)) =>: position((nextX, nextY))
  }

  def intersect(types: Seq[Type]): Type = {
    types.reduceOption(_ :&: _).getOrElse(Omega)
  }

  val moves = Map(
    "start" -> position(start),
    "up" -> intersect(possibleMoves(0, -1)),
    "down" -> intersect(possibleMoves(0, 1)),
    "left" -> intersect(possibleMoves(-1, 0)),
    "right" -> intersect(possibleMoves(1, 0))
  )
}

object Examples {
  lazy val lab1: Repository =
    new Repository(
      Array(
        Array(true, true, true, false),
        Array(false, true, true, true),
        Array(true, true, false, true),
        Array(false, true, true, true)),
      start = (0, 2),
      goal = (2, 0))

  lazy val lab2: Repository =
    new Repository(
      Array(
        Array(false, true, false),
        Array(true, true, true),
        Array(true, false, true),
        Array(true, true, true)),
      start = (0, 2),
      goal = (1, 0))

  def lab3(goalX: Int, goalY: Int) =
    new Repository(
      Array(
        Array(true, false, true, true, false),
        Array(true, false, true, false, true)),
      start = (0, 0),
      (goalX, goalY))

  lazy val lab3_1: Repository = lab3(0, 1)
  lazy val lab3_2: Repository = lab3(4, 1)
  lazy val lab3_3: Repository = lab3(2, 0)
}

