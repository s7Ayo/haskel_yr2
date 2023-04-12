  --  Function 1: Implement a function tripling a given numerical input 
  triple :: Num a => a -> a
  triple x = x * 3
  
  --  Function 2: Implement the Collatz conjecture
  collatz :: Integral a => a -> a
  collatz n
    | n == 1 = 0  -- Base case: we have reached 1
    | even n = 1 + collatz (n `div` 2)  -- n is even
    | otherwise = 1 + collatz (3 * n + 1)  -- n is odd

 
  -- Function 3: Implement a function that splits a list 
  halve :: [a] -> ([a],[a])
  halve xs = (take half xs, drop half xs)
    where 
      half = length xs `div` 2
  


  -- Function 4: Implement a function that splits a list

  isLeapYear :: Integral a => a -> Bool
  isLeapYear year
      | year `mod` 400 == 0 = True
      | year `mod` 100 == 0 = False
      | year `mod` 4  == 0 = True
      | otherwise = False
  

  -- Function 5: Implement a function that searches a list for an item
  find :: Eq a => a -> [a] -> Int -> Int
  find _ [] _ = -1
  find x (y:ys) i
      | x == y    = i
      | otherwise = find x ys (i + 1)

  findElem :: Eq a => a -> [a] -> Int
  findElem x ys = find x ys 0

  -- Function 6: Implement a pangram checker 
  type Alphabet = [Char]

  toLower :: Char -> Char
  toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

  toLowerAlphabet :: Alphabet -> Alphabet
  toLowerAlphabet = map toLower

  removeDuplicates :: Eq a => [a] -> [a]
  removeDuplicates = foldl (\acc x -> if x `elem` acc then acc else acc++[x]) []

  countLetters :: Alphabet -> Int
  countLetters = length . removeDuplicates

  isPangram :: [Char] -> Bool
  isPangram input = countLetters (toLowerAlphabet input) == 26

