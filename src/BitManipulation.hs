module BitManipulation where

import Data.Bits
import Data.Word(Word64)

type BitBoard = Word64

upsideDown :: BitBoard -> BitBoard
upsideDown  = bitSwap 0xff00ff00ff00ff00 8 
            . bitSwap 0xffff0000ffff0000 16 
            . bitSwap 0xffffffff00000000 32

bitSwap :: Bits a => a -> Int -> a -> a
bitSwap selector n x =  let mask = (x `xor` x `shift` n) .&. selector
                        in x `xor` mask `xor` mask `shift` (-n)


