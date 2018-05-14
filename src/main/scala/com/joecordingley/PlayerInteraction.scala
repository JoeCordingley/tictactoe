package com.joecordingley

import cats.effect.IO
import cats.~>
import cats.free.Free.liftF
import cats.free.Free

object PlayerInteraction {
  type PlayerFree[A] = Free[PlayerInteraction,A]

  def tell(player:Player,move:PlayerMove):PlayerFree[Unit] = liftF[PlayerInteraction,Unit](Tell(player,move))
  def tellBothPlayers(playerOne:Player,playerTwo:Player,move:PlayerMove):PlayerFree[Unit] = for { 
    _ <- tell(playerOne,move)
    _ <- tell(playerTwo,move)
  } yield ()

  def getMoveAndTell(playerToMove:Player,opponent:Player,possibleMoves:Set[Move]):PlayerFree[PlayerMove] = for {
    move <- ask(playerToMove,possibleMoves)
    playerMove = PlayerMove(playerToMove,move)
    _ <- tellBothPlayers(playerToMove, opponent, playerMove)
  } yield playerMove

  def ask(player:Player,possibleMoves:Set[Move]):PlayerFree[Move] = liftF[PlayerInteraction,Move](Ask(player,possibleMoves))

  def compiler: PlayerInteraction ~> IO = new (PlayerInteraction ~> IO) {
    def apply[A](fa: PlayerInteraction[A]): IO[A] = fa match {
      case Tell(player,move) => IO(player.update(move))
      case Ask(player,moves) => IO(player.getMove(moves))
    }
  
  }
}
sealed trait PlayerInteraction[+T]
case class Tell(player:Player,move:PlayerMove) extends PlayerInteraction[Unit]
case class Ask(player:Player,moves:Set[Move]) extends PlayerInteraction[Move]
