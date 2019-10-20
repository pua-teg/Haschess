module Lib where

import Control.Conditional
import Data.Bits
import Data.List.Split(chunksOf)
import Data.Word(Word64)
import Text.Printf

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


upsideDown :: BitBoard -> BitBoard
upsideDown  = bitSwap 0xff00ff00ff00ff00 8 
            . bitSwap 0xffff0000ffff0000 16 
            . bitSwap 0xffffffff00000000 32

bitSwap :: Bits a => a -> Int -> a -> a
bitSwap selector n x =  let mask = (x `xor` x `shift` n) .&. selector
                        in x `xor` mask `xor` mask `shift` (-n)

view :: BitBoard -> IO ()
view  = putStrLn . unlines . chunksOf 8 
      . zipWith (\n x -> bit n .&. x == 0 ? '0' ?? '1') [63, 62 .. 0] . repeat
