module BitBoard where

import Control.Conditional
import Data.Bits
import Data.List
import Data.List.Split
import Data.Word



type BitBoard = Word64


bits :: BitBoard -> [BitBoard]
bits 0 = []
bits x = let ls1b = x .&. (-x) in ls1b : bits (x `xor` ls1b)

bitSwap :: Bits a => a -> Int -> a -> a
bitSwap selector n x =  let mask = (x `xor` x `shift` n) .&. selector
                        in x `xor` mask `xor` mask `shift` (-n)

unBits :: [BitBoard] -> BitBoard
unBits = foldl1' (.|.)

upsideDown :: BitBoard -> BitBoard
upsideDown  = bitSwap 0xff00ff00ff00ff00 8 
            . bitSwap 0xffff0000ffff0000 16 
            . bitSwap 0xffffffff00000000 32

view :: BitBoard -> IO ()
view  = putStrLn . unlines . chunksOf 8 
      . zipWith (\n x -> bit n .&. x == 0 ? '0' ?? '1') [63, 62 .. 0] . repeat











