module Piece where

import BitBoard
import Direction
import Lib



data Color = White | Black deriving (Bounded, Enum, Eq, Ord, Show)
data PieceType  = Pawn
                | Knight
                | Bishop
                | Rook
                | Queen
                | King deriving (Bounded, Enum, Eq, Ord, Show)



initOf :: Color -> PieceType -> BitBoard
initOf = tableDriven [tableDriven wInits, tableDriven bInits] where
  wInits = [0xff00, 0x42, 0x24, 0x81, 0x10, 0x8]
  bInits = map upsideDown wInits

attack :: Blocker -> Color -> PieceType -> Mover -> BitBoard
attack blockers = tableDriven [
  tableDriven [ step [NE, NW]
              , step [NNE, ENE, ESE, SSE, SSW, WSW, WNW, NNW]
              , ray blockers [NE, SE, SW, NW]
              , ray blockers [NO, EA, SO, WE]
              , ray blockers [NO, NE, EA, SE, SO, SW, WE, NW]
              , step [NO, NE, EA, SE, SO, SW, WE, NW]],
  tableDriven [ step [SE, SW]
              , step [NNE, ENE, ESE, SSE, SSW, WSW, WNW, NNW]
              , ray blockers [NE, SE, SW, NW]
              , ray blockers [NO, EA, SO, WE]
              , ray blockers [NO, NE, EA, SE, SO, SW, WE, NW]
              , step [NO, NE, EA, SE, SO, SW, WE, NW]]]
