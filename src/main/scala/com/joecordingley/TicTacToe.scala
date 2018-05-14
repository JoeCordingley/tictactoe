package com.joecordingley

import cats.free.Free
import cats.free.Free.liftF
import cats.effect.IO
import cats.~>

case class TicTacToeState(playerOne:Player,playerTwo:Player,moves:List[Move]){
  def makeMove(move:Move):TicTacToeState = this.copy(moves=move::moves)
}

case class Move(rank:Rank,file:File)

sealed trait Rank

case object FirstRank extends Rank
case object SecondRank extends Rank
case object ThrirdRank extends Rank

sealed trait File

case object FirstFile extends File
case object SecondFile extends File
case object ThirdFile extends File

object TicTacToeState {
  def initial(playerOne:Player,playerTwo:Player) = new TicTacToeState(playerOne,playerTwo,Nil)
}

case class PlayerMove(player:Player,move:Move)

trait Player {
  def update(move:PlayerMove):Unit
  def getMove(moves:Set[Move]):Move
}


