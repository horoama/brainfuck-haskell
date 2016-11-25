module Main where
import Debug.Trace
import Data.Char

testbf = "++++++++++[>++++++++++<-]>++++.+++++++.--------.--."

type Pointer = Int
type Mem  = [Int]
data Inst = Incr | Decr | Next | Prev | Put | Get | While | Jmp | Nop
    deriving Show
parse :: [Char] -> Pointer -> [Inst]
parse [] i  = []
parse (x:xs) i  = case x of
    '+' -> Incr : (parse xs (i + 1))
    '-' -> Decr : (parse xs (i + 1))
    '>' -> Next : (parse xs (i + 1))
    '<' -> Prev : (parse xs (i + 1))
    '.' -> Put  : (parse xs (i + 1))
    ',' -> Get  : (parse xs (i + 1))
    '[' -> While: (parse xs (i + 1))
    ']' -> Jmp  : (parse xs (i + 1))
    _   -> Nop  : (parse xs (i + 1))

mem= replicate 256 0 :: [Int]

exec insts ip m p   | ip < length insts  = exec' $ insts !! ip
                    | otherwise = return ()
                        where   exec' Incr  = exec insts (ip + 1) incr p
                                exec' Decr  = exec insts (ip + 1) decr p
                                exec' Next  = exec insts (ip + 1) m (p + 1)
                                exec' Prev  = exec insts (ip + 1) m (p - 1)
                                exec' Put  = putStr [chr (m !! p)] >> exec insts (ip + 1) m p
                                exec' Jmp  = if (m!!p) /= 0 then exec insts (searchLeft ip) m p else exec insts (ip + 1) m p
                                exec' _  = exec insts (ip + 1) m p
                                incr = (take p m)++ [((m!!p) + 1)] ++ drop (p + 1) m
                                decr = (take p m)++ [((m!!p) - 1)] ++ drop (p + 1) m
                                searchLeft  index  =  case  insts !! index of
                                                        While   -> index + 1
                                                        _   -> searchLeft (index - 1)

main :: IO ()
main = exec (parse testbf 0) 0 mem 0
