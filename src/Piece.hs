module Piece where

import BitBoard
import Direction
import Lib

import Data.Word



type Move = Word64

data Color = White | Black deriving (Bounded, Enum, Eq, Show)

data MoveType = Quiet 
              | DoublePawnPush 
              | KingCastle
              | QueenCastle
              | Capture
              | EPCapture
              | KnightPromotion
              | BishopPromotion
              | RookPromotion
              | QueenPromotion
              | KnightPromotionCapture
              | BishopPromotionCapture
              | RookPromotionCapture
              | QueenPromotionCapture deriving (Bounded, Enum, Eq, Show)

data PieceType  = Pawn
                | Knight
                | Bishop
                | Rook
                | Queen
                | King deriving (Bounded, Enum, Eq, Show)



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

fromSquare :: Move -> BitBoard
fromSquare = bit . fromIntegral . (`shiftR` 4) . (.&. 0x03f0)

initOf :: Color -> PieceType -> BitBoard
initOf = tableDriven [tableDriven wInits, tableDriven bInits] where
  wInits = [0xff00, 0x42, 0x24, 0x81, 0x10, 0x8]
  bInits = map upsideDown wInits

moveOf :: Mover -> MoveType -> Destination -> Move
moveOf from t to  =   fromIntegral (countTrailingZeros from `shiftL` 10) 
                  .|. fromIntegral (countTrailingZeros to `shiftL` 4)
                  .|. fromIntegral (fromEnum t)

moveType :: Move -> MoveType
moveType = toEnum . fromIntegral . (.&. 0x000f)

toSquare :: Move -> BitBoard
toSquare = bit . fromIntegral . (`shiftR` 10) . (.&. 0xfc00)
