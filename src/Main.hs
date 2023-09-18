module Main where

import Control.Monad.State   (evalStateT)
import Data.Vector as Vector (replicate)
import Sass.X8.Model         (Memory (Memory), MemoryAddress (MemoryAddress),
                              outputAddress, Operation (Mov, Const, Return))
import Sass.X8.Run           (runProgram)

main :: IO ()
main = evalStateT (runProgram [Mov outputAddress (MemoryAddress 4), Return, Const $ MemoryAddress 100]) (Memory (Vector.replicate 256 0), MemoryAddress 0)
