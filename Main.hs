import Control.Monad (ap, forM)
import Data.List

repeatString n str = repeatHelper n str ""

repeatHelper 0 _ result = result
repeatHelper n str result = repeatHelper (n - 1) str (result ++ str)

fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a + b) (n - 1)

factorial n
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i

safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)

headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)

stringOnly :: Either Int String -> String
stringOnly (Right str) = str
stringOnly (Left num) = show num

parseCountry :: String -> Maybe String
parseCountry "FI" = Just "Finland"
parseCountry "SE" = Just "Sweden"
parseCountry _ = Nothing

flyTo :: String -> String
flyTo countryCode =
  case parseCountry countryCode of
    Just country -> "You're flying to " ++ country
    Nothing -> "You're going nowhere"

distanceToSunday :: String -> Int
distanceToSunday d = case d of
  "Monday" -> 6
  "Tuesday" -> 5
  "Wednesday" -> 4
  "Thursday" -> 3
  "Friday" -> 2
  "Saturday" -> 1
  "Sunday" -> 0

motivate :: String -> String
motivate day = case distanceToSunday day of
  6 -> "Have a nice day at work!"
  5 -> "You're closer to Sunday now"
  n ->
    if n > 1
      then show (n - 1) ++ " more day(s) until weekend"
      else "Relax, you don't need to work today"

addThree :: Int -> Int
addThree x = x + 3

doTwice :: (a -> a) -> a -> a
doTwice f = f . f

positive :: Int -> Bool
positive x = x > 0

onlyPositive = filter positive

wrapJust = map Just

palindrome :: String -> Bool
palindrome s = s == reverse s

palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1 .. n])

substringsOfLength :: Int -> String -> [String]
substringsOfLength n s = map shorten (tails s)
  where
    shorten = take n

whatFollows :: Char -> Int -> String -> [[Char]]
whatFollows c k = map tail . filter ((== [c]) . take 1) . map (take (k + 1)) . tails

between :: Integer -> Integer -> Integer -> Bool
between l h x = l < x && x < h

findSubstring :: String -> String -> String
findSubstring chars = takeWhile (`elem` chars) . dropWhile (\x -> not $ elem x chars)

iterBuild f 0 x = [x]
iterBuild f n x = x : iterBuild f (n - 1) (f x)

split :: Char -> String -> [String]
split c [] = []
split c xs = start : split c (drop 1 rest)
  where
    start = takeWhile (/= c) xs
    rest = dropWhile (/= c) xs

firstLetters s = [c | (c : _) <- words s]

bothPairsEqual :: (Eq a, Eq b) => a -> a -> b -> b -> Bool
bothPairsEqual a b x y = a == b && x == y

myMax :: [Int] -> Int
myMax [] = 0
myMax (x : xs) = go x xs
  where
    go biggest [] = biggest
    go biggest (x : xs) = go (max biggest x) xs

data Color = Red | Green | Blue

rgb :: Color -> [Double]
rgb Red = [1, 0, 0]
rgb Green = [0, 1, 0]
rgb Blue = [0, 0, 1]

data Card = Joker | Club Int | Heart Int | Spade Int | Diamond Int
  deriving (Show)

data Described a = Describe a String

data Tree a = Empty | Node a (Tree a) (Tree a)

exampleTree :: Tree Int
exampleTree =
  Node
    0
    ( Node
        1
        (Node 2 Empty Empty)
        (Node 3 Empty Empty)
    )
    (Node 4 Empty Empty)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

data Person = MkPerson {name :: String, town :: String, age :: Int}
  deriving (Show)

class Size a where
  empty :: a
  size :: a -> Int
  sameSize :: a -> a -> Bool

instance Size (Maybe a) where
  empty = Nothing
  size Nothing = 0
  size (Just a) = 1
  sameSize x y = size x == size y

instance Size [a] where
  empty = []
  size xs = length xs
  sameSize xs ys = length xs == length ys

class Example a where
  example :: a
  examples :: [a]
  examples = [example]

instance Example Int where
  example = 1
  examples = [0, 1, 2]

instance Example Bool where
  example = True

data DerivedPerson = Dead | Alive String Int
  deriving (Show, Eq, Ord)

data Pair a = Pair a a
  deriving (Show)

instance (Eq a) => Eq (Pair a) where
  (Pair a b) == (Pair x y) = x == a && y == b

strictHead :: [a] -> a
strictHead xs = seq (last xs) (head xs)

newtype Inverted = Inverted Int
  deriving (Show, Eq)

instance Ord Inverted where
  compare (Inverted i) (Inverted j) = compare j i

questionnaire = do
  putStrLn "Write down youf lucky number"
  s <- getLine
  putStrLn $ "Haha, I know your secret now: " ++ s

writeSomething = do
  putStrLn "Give me a line"
  getLine

ask :: [String] -> IO [String]
ask questions = do
  forM questions askOne

askOne :: String -> IO String
askOne q = do
  putStr q
  putStrLn "?"
  getLine

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x ?> f = f x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

safeNth :: Int -> [a] -> Maybe a
safeNth 0 xs = safeHead xs
safeNth n xs = safeTail xs ?> safeNth (n - 1)

data State s a = State (s -> (a, s))

runState (State f) s = f s

put :: s -> State s ()
put state = State (\old -> ((), state))

get :: State s s
get = State (\s -> (s, s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

instance Functor (State s) where
  fmap f (State g) = State (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State (\s -> (x, s))
  op >>= f = State h
    where
      h state0 =
        let (val1, state1) = runState op state0
            op2 = f val1
         in runState op2 state1

main = do
  print "Blah"
