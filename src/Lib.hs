module Lib where

import Control.Conditional
import Data.Bits
import Data.List.Split(chunksOf)
import Data.Word(Word64)
import Text.Printf

import BitManipulation

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type BitBoard = Word64

data PieceType  = Pawn
                | Knight
                | Bishop
                | Rook
                | Queen
                | King deriving (Bounded, Enum, Eq, Ord, Show)

data Color = White | Black deriving (Bounded, Enum, Eq, Ord, Show)

listAll :: (Bounded a, Ord a, Enum a) => [a]
listAll = [minBound .. maxBound]

tableDriven :: (Bounded a, Ord a, Enum a, Show a) => [b] -> a -> b
tableDriven xs a = maybe outOfBounds id (lookup a (zip listAll xs)) where
  outOfBounds = error (printf "INVALID TABLE: No value assigned to the key \"%s\"." (show a))

initOf :: Color -> PieceType -> BitBoard
initOf = tableDriven [tableDriven wInits, tableDriven bInits] where
  wInits = [0xff00, 0x42, 0x24, 0x81, 0x10, 0x8]
  bInits = map upsideDown wInits

view :: BitBoard -> IO ()
view  = putStrLn . unlines . chunksOf 8 
      . zipWith (\n x -> bit n .&. x == 0 ? '0' ?? '1') [63, 62 .. 0] . repeat

attack :: BitBoard -> BitBoard -> Color -> PieceType -> BitBoard
attack pieces mover = tableDriven [
  tableDriven [ (\x -> x `shift` 7 .&. 0x7f7f7f7f7f7f7f7f .|. x `shift` 9 .&. 0xfefefefefefefefe)
              , (\x ->  x `shift` 15 .&. 0x7f7f7f7f7f7f7f7f
                    .|. x `shift` 6 .&. 0x3f3f3f3f3f3f3f3f
                    .|. x `shift` (-10) .&. 0x3f3f3f3f3f3f3f3f
                    .|. x `shift` (-17) .&. 0x7f7f7f7f7f7f7f7f
                    .|. x `shift` (-15) .&. 0xfefefefefefefefe
                    .|. x `shift` (-6) .&. 0xfcfcfcfcfcfcfcfc
                    .|. x `shift` 10 .&. 0xfcfcfcfcfcfcfcfc
                    .|. x `shift` 17 .&. 0xfefefefefefefefe)
              , (\x -> TODO)
              , (\x -> TODO)
              , (\x -> TODO)
              , (\x ->  x `shift` 8 
                    .|. x `shift` 7 .&. 0x7f7f7f7f7f7f7f7f
                    .|. x `shift` (-1) .&. 0x7f7f7f7f7f7f7f7f
                    .|. x `shift` (-9) .&. 0x7f7f7f7f7f7f7f7f
                    .|. x `shift` (-7) .&. 0xfefefefefefefefe
                    .|. x `shift` 1 .&. 0xfefefefefefefefe
                    .|. x `shift` 9 .&. 0xfefefefefefefefe
                    .|. x `shift` (-8))
  ],
  tableDriven [TODO]]
