module Sass.X8.Model where

import Data.Word   (Word8)
import Data.Vector (Vector, (//), (!))

data MemoryAddress = MemoryAddress Word8 deriving (Eq, Show)

inputAddress :: MemoryAddress
inputAddress = MemoryAddress $ 253

outputAddress :: MemoryAddress
outputAddress = MemoryAddress $ 254

exitAddress :: MemoryAddress
exitAddress = MemoryAddress $ 255

data Operation = Add   MemoryAddress MemoryAddress
               | Sub   MemoryAddress MemoryAddress
               | Mul   MemoryAddress MemoryAddress
               | Div   MemoryAddress MemoryAddress
               | Mod   MemoryAddress MemoryAddress
               | And   MemoryAddress MemoryAddress
               | Xor   MemoryAddress MemoryAddress
               | Or    MemoryAddress MemoryAddress
               | Not   MemoryAddress
               | Jmp   MemoryAddress
               | Je    MemoryAddress MemoryAddress MemoryAddress
               | Jne   MemoryAddress MemoryAddress MemoryAddress
               | Jl    MemoryAddress MemoryAddress MemoryAddress
               | Jle   MemoryAddress MemoryAddress MemoryAddress
               | Jg    MemoryAddress MemoryAddress MemoryAddress
               | Jge   MemoryAddress MemoryAddress MemoryAddress
               | Mov   MemoryAddress MemoryAddress
               | Const MemoryAddress
               | Return
               | Nop
    deriving Show

getInAddress :: Operation -> Maybe MemoryAddress
getInAddress (Add _ b)   = Just b
getInAddress (Sub _ b)   = Just b
getInAddress (Mul _ b)   = Just b
getInAddress (Div _ b)   = Just b
getInAddress (Mod _ b)   = Just b
getInAddress (And _ b)   = Just b
getInAddress (Xor _ b)   = Just b
getInAddress (Or _ b)    = Just b
getInAddress (Not _)     = Nothing
getInAddress (Jmp _)     = Nothing
getInAddress (Je _ _ _)  = Nothing
getInAddress (Jne _ _ _) = Nothing
getInAddress (Jl _ _ _)  = Nothing
getInAddress (Jle _ _ _) = Nothing
getInAddress (Jg _ _ _)  = Nothing
getInAddress (Jge _ _ _) = Nothing
getInAddress (Mov _ b)   = Just b
getInAddress (Const _)   = Nothing
getInAddress Return      = Nothing
getInAddress Nop         = Nothing

getOutAddress :: Operation -> Maybe MemoryAddress
getOutAddress (Add a _)   = Just a
getOutAddress (Sub a _)   = Just a
getOutAddress (Mul a _)   = Just a
getOutAddress (Div a _)   = Just a
getOutAddress (Mod a _)   = Just a
getOutAddress (And a _)   = Just a
getOutAddress (Xor a _)   = Just a
getOutAddress (Or a _)    = Just a
getOutAddress (Not _)     = Nothing
getOutAddress (Jmp _)     = Nothing
getOutAddress (Je _ _ _)  = Nothing
getOutAddress (Jne _ _ _) = Nothing
getOutAddress (Jl _ _ _)  = Nothing
getOutAddress (Jle _ _ _) = Nothing
getOutAddress (Jg _ _ _)  = Nothing
getOutAddress (Jge _ _ _) = Nothing
getOutAddress (Mov a _)   = Just a
getOutAddress (Const _)   = Nothing
getOutAddress Return      = Nothing
getOutAddress Nop         = Nothing

data Memory = Memory (Vector Word8) deriving Show

get :: Memory -> MemoryAddress -> Word8
get (Memory v) (MemoryAddress i) = v ! (fromIntegral i)

update :: Memory -> MemoryAddress -> Word8 -> Memory
update (Memory m) (MemoryAddress i) v = Memory $ m // [(fromIntegral i, v)]

getOperation :: Memory -> MemoryAddress -> Operation
getOperation m a@(MemoryAddress i) = case get m a of
    0  -> Add (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    1  -> Sub (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    2  -> Mul (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    3  -> Div (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    4  -> Mod (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    5  -> And (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    6  -> Xor (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    7  -> Or  (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    8  -> Not (MemoryAddress $ get m (MemoryAddress $ i + 1))
    9  -> Jmp (MemoryAddress $ get m (MemoryAddress $ i + 1))
    10 -> Je  (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
              (MemoryAddress $ get m (MemoryAddress $ i + 3))
    11 -> Jne (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
              (MemoryAddress $ get m (MemoryAddress $ i + 3))
    12 -> Jl  (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
              (MemoryAddress $ get m (MemoryAddress $ i + 3))
    13 -> Jle (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
              (MemoryAddress $ get m (MemoryAddress $ i + 3))
    14 -> Jg  (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
              (MemoryAddress $ get m (MemoryAddress $ i + 3))
    15 -> Jge (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
              (MemoryAddress $ get m (MemoryAddress $ i + 3))
    16 -> Mov (MemoryAddress $ get m (MemoryAddress $ i + 1))
              (MemoryAddress $ get m (MemoryAddress $ i + 2))
    17 -> Return
    _  -> Nop

updateOperation :: Memory -> MemoryAddress -> Operation -> Memory
updateOperation m a@(MemoryAddress i) o = case o of
    Add (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 0) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Sub (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 1) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Mul (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 2) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Div (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 3) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Mod (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 4) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    And (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 5) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Xor (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 6) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Or  (MemoryAddress x) (MemoryAddress y) -> 
        update (update (update m a 7) (MemoryAddress $ i + 1) x) 
               (MemoryAddress $ i + 2) y
    Not (MemoryAddress x) -> update (update m a 8) (MemoryAddress $ i + 1) x
    Jmp (MemoryAddress x) -> update (update m a 9) (MemoryAddress $ i + 1) x
    Je  (MemoryAddress x) (MemoryAddress y) (MemoryAddress z) -> 
        update (update (update (update m a 10) (MemoryAddress $ i + 1) x)
                       (MemoryAddress $ i + 2) y) (MemoryAddress $ i + 3) z
    Jne (MemoryAddress x) (MemoryAddress y) (MemoryAddress z) -> 
        update (update (update (update m a 11) (MemoryAddress $ i + 1) x)
                       (MemoryAddress $ i + 2) y) (MemoryAddress $ i + 3) z
    Jl  (MemoryAddress x) (MemoryAddress y) (MemoryAddress z) -> 
        update (update (update (update m a 12) (MemoryAddress $ i + 1) x)
                       (MemoryAddress $ i + 2) y) (MemoryAddress $ i + 3) z
    Jle (MemoryAddress x) (MemoryAddress y) (MemoryAddress z) -> 
        update (update (update (update m a 13) (MemoryAddress $ i + 1) x)
                       (MemoryAddress $ i + 2) y) (MemoryAddress $ i + 3) z
    Jg  (MemoryAddress x) (MemoryAddress y) (MemoryAddress z) -> 
        update (update (update (update m a 14) (MemoryAddress $ i + 1) x)
                       (MemoryAddress $ i + 2) y) (MemoryAddress $ i + 3) z
    Jge (MemoryAddress x) (MemoryAddress y) (MemoryAddress z) -> 
        update (update (update (update m a 15) (MemoryAddress $ i + 1) x)
                       (MemoryAddress $ i + 2) y) (MemoryAddress $ i + 3) z
    Mov (MemoryAddress x) (MemoryAddress y) ->
        update (update (update m a 16) (MemoryAddress $ i + 1) x)
               (MemoryAddress $ i + 2) y
    Const (MemoryAddress x) -> update m a x
    Return                  -> update m a 17
    Nop                     -> update m a 255
