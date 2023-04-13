module A1E1_21484872 (
  triple,
  collatz,
  halve,
  isLeapYear,
  find,
  isPangram
) where
  
  
  
  
  
  
  --  Function 1: Implement a function tripling a given numerical input 
  triple :: Num a => a -> a
  triple x = x * 3 -- x is tripled 
  
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
  searchs :: Eq a => a -> [a] -> Int -> Int
  searchs _ [] _ = -1
  searchs x (y:ys) i
      | x == y    = i
      | otherwise = searchs x ys (i + 1)

  find :: Eq a => a -> [a] -> Int
  find x ys = searchs x ys 0

  -- Function 6: Implement a pangram checker 
  isPangram :: String -> Bool
  isPangram s = all (`elem` map toLower s) ['a'..'z']
    where
      toLower c
        | c `elem` ['A'..'Z'] = toEnum (fromEnum c + 32)
        | otherwise = c
    
