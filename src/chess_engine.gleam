import gleam/io
import gleam/int
import gleam/map.{Map}
import gleam/bitwise

// A type to represent each piece to facilitate pattern matching.
type PieceType {
  WKing
  WQueen
  WRook
  WKnight
  WBishop
  WPawn
  BKing
  BQueen
  BRook
  BKnight
  BBishop
  BPawn
}

// Associate a Bitboard to each piece type.
type PieceInfo {
  PieceInfo(current_pos: Bitboard)
}

type Bitboard = Int

// Each position is a mapping from a Piece type to all the required info about
// that piece.
type Position = Map(PieceType, PieceInfo)

// Coordinates on the board i.e. a1, b3 etc.
type Coordinate = String

// The tree that will be used to find the next best move.
type  MoveTree {
  Node(position: Position, children: List(Position), score: Float)  
  Leaf
}

pub fn main() {
  let t0 = 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
  let t = 0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_00000000
  let t1 = 0b0000000000000000000000000000000000000000000000000000000000000011
  io.debug(int.to_base2(bitwise.or(t, t1)))
  io.debug(int.to_base2(bitwise.shift_left(t, 8)))
}
