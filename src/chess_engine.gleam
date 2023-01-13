import gleam/io
import gleam/int
import gleam/bitwise

type Bitboard {
    Bitboard(board_state: Int)
}

type PieceInfo {
  WKing,
  WQueen,
  WRook,
  WKnight,
  WBishop,
  WPawn,
  BKing,
  BQueen,
  BRook,
  BKnight,
  BBishop,
  BPawn,
}

pub fn main() {
  let t0 = 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000
  let t = 0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_00000000
  let t1 = 0b0000000000000000000000000000000000000000000000000000000000000011
  io.debug(int.to_base2(bitwise.or(t, t1)))
  io.debug(int.to_base2(bitwise.shift_left(t, 8)))
}
