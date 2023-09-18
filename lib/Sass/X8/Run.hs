{-# LANGUAGE FlexibleContexts                  #-}

module Sass.X8.Run where

import Control.Monad.State    as State (MonadState, get, put)
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Data.Bits                       ((.&.), (.|.), xor)
import Data.Word                       (Word8)
import Sass.X8.Model          as Model (Memory, MemoryAddress (MemoryAddress), 
                                        Operation (Add, Sub, Mul, Div, Mod, 
                                        And, Xor, Or, Not, Jmp, Je, Jne, Jl, 
                                        Jle, Jg, Jge, Mov, Const, Nop, Return), 
                                        getInAddress, getOutAddress, 
                                        inputAddress, outputAddress, 
                                        exitAddress, get, update, getOperation,
                                        updateOperation)

type State = (Memory, MemoryAddress)

makeOperation :: (MonadState State m, MonadIO m) => Operation -> m (Maybe Word8)
makeOperation o = case o of
    Add a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) + (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Sub a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) - (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Mul a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) * (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Div a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) `div` (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Mod a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) `mod` (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    And a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) .&. (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Xor a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) `xor` (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Or  a b   -> do
        (m, MemoryAddress i) <- State.get
        let res = (Model.get m a) .|. (Model.get m b)
        put (update m a res, MemoryAddress $ i + 3)
        return $ Just res
    Not a     -> do
        (m, MemoryAddress i) <- State.get
        let res = negate $ Model.get m a
        put (update m a res, MemoryAddress $ i + 2)
        return $ Just res
    Jmp p     -> do
        (m, _) <- State.get
        put (m, p)
        return Nothing
    Je  a b p -> do
        (m, MemoryAddress i) <- State.get
        if (Model.get m a) == (Model.get m b) 
            then put (m, p)
            else put (m, MemoryAddress $ i + 4)
        return Nothing
    Jne a b p -> do
        (m, MemoryAddress i) <- State.get
        if (Model.get m a) /= (Model.get m b) 
            then put (m, p)
            else put (m, MemoryAddress $ i + 4)
        return Nothing
    Jl  a b p -> do
        (m, MemoryAddress i) <- State.get
        if (Model.get m a) < (Model.get m b) 
            then put (m, p)
            else put (m, MemoryAddress $ i + 4)
        return Nothing
    Jle a b p -> do
        (m, MemoryAddress i) <- State.get
        if (Model.get m a) <= (Model.get m b) 
            then put (m, p)
            else put (m, MemoryAddress $ i + 4)
        return Nothing
    Jg  a b p -> do
        (m, MemoryAddress i) <- State.get
        if (Model.get m a) > (Model.get m b) 
            then put (m, p)
            else put (m, MemoryAddress $ i + 4)
        return Nothing
    Jge a b p -> do
        (m, MemoryAddress i) <- State.get
        if (Model.get m a) >= (Model.get m b) 
            then put (m, p)
            else put (m, MemoryAddress $ i + 4)
        return Nothing
    Mov x y   -> do
        (m, MemoryAddress i) <- State.get
        let res = Model.get m y
        put (update m x res, MemoryAddress $ i + 3)
        return $ Just res
    Const _ -> do
        (m, MemoryAddress i) <- State.get
        put (m, MemoryAddress $ i + 1)
        return Nothing
    Return -> do
        (m, _) <- State.get
        put (m, exitAddress)
        return Nothing
    Nop -> do
        (m, MemoryAddress i) <- State.get
        put (m, MemoryAddress $ i + 1)
        return Nothing

type Program = [Operation]

runOperation :: (MonadState State m, MonadIO m) => m ()
runOperation = do
    (m, a) <- State.get
    if a == exitAddress
        then return ()
        else let op = getOperation m a in do
            if maybe False (\add -> add == inputAddress) (getInAddress op)
                then do
                    input  <- liftIO $ readLn
                    put (update m inputAddress input, a)
                else return ()
            resM <- makeOperation op
            let res = maybe 0 (\x -> x) resM 
            if maybe False (\add -> add == outputAddress) (getOutAddress op)
                then liftIO $ print res
                else return ()
            runOperation

addOpSize :: MemoryAddress -> Operation -> MemoryAddress
addOpSize (MemoryAddress i) op = case op of 
    Add _ _   -> MemoryAddress $ i + 3
    Sub _ _   -> MemoryAddress $ i + 3
    Mul _ _   -> MemoryAddress $ i + 3
    Div _ _   -> MemoryAddress $ i + 3
    Mod _ _   -> MemoryAddress $ i + 3
    And _ _   -> MemoryAddress $ i + 3
    Xor _ _   -> MemoryAddress $ i + 3
    Or _ _    -> MemoryAddress $ i + 3
    Not _     -> MemoryAddress $ i + 2
    Jmp _     -> MemoryAddress $ i + 2
    Je _ _ _  -> MemoryAddress $ i + 4
    Jne _ _ _ -> MemoryAddress $ i + 4
    Jl _ _ _  -> MemoryAddress $ i + 4
    Jle _ _ _ -> MemoryAddress $ i + 4
    Jg _ _ _  -> MemoryAddress $ i + 4
    Jge _ _ _ -> MemoryAddress $ i + 4
    Mov _ _   -> MemoryAddress $ i + 3
    Const _   -> MemoryAddress $ i + 2
    Return    -> MemoryAddress $ i + 1
    Nop       -> MemoryAddress $ i + 1

runProgramHelper :: (MonadState State m, MonadIO m) => Program -> MemoryAddress -> m ()
runProgramHelper [] _ = do
    (m, _) <- State.get
    put (m, MemoryAddress 0)
    runOperation
runProgramHelper (op: ops) i = do
    (m, a) <- State.get
    put (updateOperation m i op, a)
    runProgramHelper ops (addOpSize i op)

runProgram :: (MonadState State m, MonadIO m) => Program -> m ()
runProgram p = runProgramHelper p (MemoryAddress 0)
