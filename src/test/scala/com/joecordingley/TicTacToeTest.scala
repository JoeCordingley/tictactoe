package com.joecordingley

import org.scalatest.{FreeSpec, Matchers}
import cats.data.Writer
import cats.~>
import cats.implicits._
import cats.Id

class TicTacToeTest extends FreeSpec with Matchers {

  def dummyPlayer = new Player {
    def update(move:PlayerMove):Unit = ???
    def getMove(moves:Set[Move]):Move = ???
  }
  val playerOne = dummyPlayer
  val playerTwo = dummyPlayer
  val initial = TicTacToeState.initial(playerOne,playerTwo)
  val move = new Move(FirstRank,SecondFile)
  val playerMove = new PlayerMove(playerOne,move)

  type WriterLog = List[PlayerInteraction[Any]]
  type TestWriter[A] = Writer[WriterLog,A]

  def testCompiler(fk:PlayerInteraction~>Id): PlayerInteraction ~> TestWriter = new (PlayerInteraction ~> TestWriter) {
    def apply[A](fa:PlayerInteraction[A]):TestWriter[A] = Writer.tell[WriterLog](List(fa)).map(_ =>fk.apply[A](fa))
  }
  def fullTestCompiler:PlayerInteraction ~> TestWriter = testCompiler(getResponse)

  def getResponse:PlayerInteraction ~> Id = new(PlayerInteraction ~> Id){
    def apply[A](fa:PlayerInteraction[A]):Id[A] = fa match {
      case t:Tell => ()
      case a:Ask => a.moves.head
    }
  }

//  type TestLog[C[_]] = List[C[Any]] //TODO One day maybe
//  type AbstractTestWriter[C[_],A] = Writer[TestLog[C],A]
//  def abstractTestCompiler[C[_],B](response:C[A] =>A forSome {type A}): C ~> AbstractTestWriter[C,?] = new (C ~> AbstractTestWriter[C,?]){
//    def apply[A](fa:C[A]):AbstractTestWriter[C,A] = Writer.tell[TestLog[C]](List(fa)).map(_ =>response)
//  }

  "a tictactoe board" - {
    "should be initialized blank" in {
      initial.moves should be ( Nil )
    }
    "should be able to append a move" in {
      initial.makeMove(move).moves should be (List(move))
    }
  }
  "update players" - {
    "should be able to update players" in { 
      PlayerInteraction.tellBothPlayers(playerOne,playerTwo,playerMove).foldMap(fullTestCompiler).written should be (List(Tell(playerOne,playerMove),Tell(playerTwo,playerMove)))
    }
  }

}
