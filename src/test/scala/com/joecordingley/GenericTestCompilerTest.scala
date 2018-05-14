package com.joecordingley

import org.scalatest.{FreeSpec, Matchers}
import cats.data.Writer
import cats.~>
import cats.implicits._
import cats.Id

class GenericTestCompilerTest extends FreeSpec with Matchers {

  type WriterLog = List[Any]
  type TestWriter[A] = Writer[WriterLog,A]

  def testCompiler[F[_]](response:F~>Id): F ~> TestWriter = new (F ~> TestWriter) {
    def apply[A](fa:F[A]):TestWriter[A] = Writer(List(fa),response(fa))
  }
  

  "getMoveAndTell" - {
    def dummyPlayer = new Player {
      def update(move:PlayerMove):Unit = ???
      def getMove(moves:Set[Move]):Move = ???
    }
    val playerOne = dummyPlayer
    val playerTwo = dummyPlayer
    val move1 = new Move(FirstRank,SecondFile)
    val playerMove1 = new PlayerMove(playerOne,move1)
    val move2 = new Move(FirstRank,SecondFile)
    val moves = Set(move1,move2)
    val f = PlayerInteraction.getMoveAndTell(playerOne,playerTwo,moves)

    "when asking my preprogrammed players" - {
      val responses: PlayerInteraction ~> Id = new ( PlayerInteraction ~> Id ) {
        def apply[A](fa:PlayerInteraction[A]) = fa match {
          case _:Tell => ()
          case _:Ask => move1
        }
      }
      val myTestCompiler: PlayerInteraction ~> TestWriter = testCompiler(responses)
      "should tell them both of the move and return it" in {
        val expectedInteractions = List(Ask(playerOne,moves),Tell(playerOne,playerMove1),Tell(playerTwo,playerMove1))
        val expectedReturn = playerMove1
        val (interactions,returnVal) = f.foldMap(myTestCompiler).run
        interactions should equal ( expectedInteractions )
        returnVal should equal ( expectedReturn )
      }
    }
  }
}
