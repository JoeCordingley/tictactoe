package com.joecordingley

import org.scalatest.{FreeSpec, Matchers}
import cats.data.Writer
import cats.~>
import cats.implicits._
import cats.Id
import cats.data.State._
import cats.data.State

class GenericTestCompilerTest extends FreeSpec with Matchers {

  type WriterLog = List[Any]
  type TestWriter[A] = Writer[WriterLog,A]

  def testCompiler[F[_]](response:F~>Id): F ~> TestWriter = new (F ~> TestWriter) {
    def apply[A](fa:F[A]):TestWriter[A] = Writer(List(fa),response(fa))
  }

  case class TestState[F[_]](responses:List[F ~> Id],interactions: List[Any])
  type StateLog[F[_],A] = State[TestState[F], A]
  def testCompiler2[F[_]]: F ~> StateLog[F,?] = new (F ~> StateLog[F,?]){
    def apply[A](fa:F[A]):StateLog[F,A] = for {
      responses <- State.inspect[TestState[F],List[F~>Id]](_.responses)
      interactions <- State.inspect[TestState[F],List[Any]](_.interactions)
      val a = responses.head[A](fa)
      _ <- set[TestState[F]](TestState[F](responses.tail,fa::interactions))
    } yield a
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
    val move2 = new Move(SecondRank,ThirdFile)
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
    "when asking new preprogrammed players" - {
      val tellResponse: PlayerInteraction ~> Id = new (PlayerInteraction ~> Id) {
        def apply[A](fa:PlayerInteraction[A]) = fa match {
          case _: Tell => ()
          case _ => throw new Exception("test failed")
        }
      }
      val askResponse: PlayerInteraction ~> Id = new (PlayerInteraction ~> Id) {
        def apply[A](fa:PlayerInteraction[A]) = fa match {
          case _: Ask => move1 
          case _ => throw new Exception("test failed")
        }
      }
      val responses: List[PlayerInteraction ~> Id] = List(
        askResponse,
        tellResponse,
        tellResponse
      )
      type PlayerState[A] = StateLog[PlayerInteraction,A]
      val myTestCompiler: PlayerInteraction ~> PlayerState = testCompiler2[PlayerInteraction]
      "should tell them both of the move and return it" in {
        val expectedInteractions = List(Ask(playerOne,moves),Tell(playerOne,playerMove1),Tell(playerTwo,playerMove1))
        val expectedReturn = playerMove1
        val (TestState(_,interactions),returnVal) = f.foldMap(myTestCompiler).run(TestState(responses,Nil)).value
        interactions.reverse should equal ( expectedInteractions )
        returnVal should equal ( expectedReturn )
      }
    }
  }
}
