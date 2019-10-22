module Direction where

import BitBoard
import Lib

import Data.Bits

type Blocker  = BitBoard
type Mover    = BitBoard
type Filled   = BitBoard
type Destination = BitBoard

data Direction  = NO | NNE | NE | ENE | EA | ESE | SE | SSE 
                | SO | SSW | SW | WSW | WE | WNW | NW | NNW 
                deriving (Bounded, Enum, Eq, Ord, Show)



fill :: Blocker -> Direction -> Mover -> Filled
fill blockers direction x = occluded `shift` n .&. wa where
  n = bitPos direction
  wa = wrapAvoider direction
  occluded = fill' (complement blockers .&. wa) [n, 2 * n, 4 * n] x
  fill' _ [] x        = x
  fill' p (n : ns) x  = fill' (p .&. p `shift` n) ns (x .|. x `shift` n .&. p)

move :: Direction -> Mover -> BitBoard
move d = (.&. wrapAvoider d) . (`shift` bitPos d)

ray :: Blocker -> [Direction] -> Mover -> BitBoard
ray b ds = unBits . (sequence . map (fill b) $ ds)

rays :: Blocker -> [Direction] -> Mover -> [Destination]
rays b ds = concatMap bits . (sequence . map (fill b) $ ds)

step :: [Direction] -> Mover -> BitBoard
step ds = unBits . (steps ds)

steps :: [Direction] -> Mover -> [Destination] 
steps = sequence . map move



bitPos :: Direction -> Int
bitPos = tableDriven  [ 8, 15, 7, 6, (-1), (-10), (-9), (-17)
                      , (-8), (-15), (-7), (-6), 1, 10, 9, 17]

notAFile, notHFile, notABFile, notGHFile :: BitBoard
notAFile  = 0x7f7f7f7f7f7f7f7f
notHFile  = 0xfefefefefefefefe
notABFile = 0x3f3f3f3f3f3f3f3f
notGHFile = 0xfcfcfcfcfcfcfcfc

wrapAvoider :: Direction -> BitBoard
wrapAvoider = select . (`mod` 8) . bitPos where
  select = tableDrivenBy [0, 1, 2, 6, 7]
    [0xffffffffffffffff, notHFile, notGHFile, notABFile, notAFile]
