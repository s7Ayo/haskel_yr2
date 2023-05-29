-- Function 1: Merge two sorted lists

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys


-- Function 2: Encrypt text using Caesar’s cipher

alphabet = [ 'a' .. 'z' ] ++ [' '] ++ [ 'A' .. 'Z' ] ++ [ '1' .. '9' ] ++ ['0']

shiftChar :: Int -> Char -> Char
shiftChar n c = case lookup c (zip alphabet (drop n (cycle alphabet))) of
                   Just x  -> x
                   Nothing -> c

encrypt :: [Char] -> Int -> [Char]
encrypt text n = map (shiftChar n) text

decrypt :: [Char] -> Int -> [Char]
decrypt text n = map (shiftChar (negate n)) text


-- Function 3: Define functions and data types for stock-keeping
data Outcome a b = Error a | Completion b deriving (Show, Eq)

instance Functor (Outcome a) where
  fmap _ (Error x) = Error x
  fmap f (Completion y) = Completion (f y)

instance Applicative (Outcome a) where
  pure = Completion
  Error x <*> _ = Error x
  Completion f <*> r = fmap f r

instance Monad (Outcome a) where
  Error x >>= _ = Error x
  Completion y >>= k = k y

changeStock :: Ord b => Num b => b -> b -> Outcome [Char] b
changeStock n stock
  | stock + n < 0 = Error "Negative stock level"
  | otherwise     = Completion (stock + n)

increaseStock :: Ord b => Num b => b -> b -> Outcome [Char] b
increaseStock = changeStock

decreaseStock :: Ord b => Num b => b -> b -> Outcome [Char] b
decreaseStock n = changeStock (negate n)
