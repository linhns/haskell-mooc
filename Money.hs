module Main where

import Control.Applicative (Applicative (liftA2))
import Text.Read (readMaybe)

data Currency = EUR | USD
  deriving (Show, Eq)

data Money = Money Int Currency
  deriving (Show, Eq)

parseCurrency :: String -> Maybe Currency
parseCurrency "e" = pure EUR
parseCurrency "$" = pure USD
parseCurrency "€" = pure EUR
parseCurrency _ = Nothing

parseAmount :: String -> Maybe Int
parseAmount = readMaybe

parseMoney :: String -> String -> Maybe Money
parseMoney amount curr =
  liftA2 Money (parseAmount amount) (parseCurrency curr)

say :: String -> Int -> String
say x i = x ++ "=" ++ show i

main = do
  print $ parseMoney "4x" "e"
  print $ parseMoney "4" "$"
  print $ liftA2 (+) [1, 2, 3] [4, 5]
  print $ say <$> Just "three" <*> Just 3
