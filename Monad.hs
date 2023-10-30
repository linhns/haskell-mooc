module Main where

import Control.Monad.State
import Data.List
import Text.Read (readMaybe)

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop

stackStuff = do
  a <- pop
  if a == 7
    then push 7
    else do
      push 3
      push 11

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  when (a == 100) stackStuff

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
    then put [8, 9, 10]
    else put [6, 7, 8]

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM go [] (words st)
  pure result

go :: [Double] -> String -> Maybe [Double]
go (x : y : xs) "*" = return $ (x * y) : xs
go (x : y : xs) "+" = return $ (x + y) : xs
go (x : y : xs) "-" = return $ (y - x) : xs
go xs num = fmap (: xs) (readMaybe num)

main = do
  print $ runState stackManip [5, 3, 4, 1]
  print $ runState stackStuff [7, 3, 4, 1]
  print $ runState stackStuff [5, 3, 4, 1]
  print $ runState moreStack [100, 3, 4, 1]
  print $ runState stackyStack [100, 3, 4, 1]
  print $ foldM binSmalls 0 [1, 2, 4, 5]
  print $ solveRPN "1 2 * 4 + 5 *"
