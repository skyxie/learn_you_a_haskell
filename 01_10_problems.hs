-- Solutions to 99 Questions: https://wiki.haskell.org/99_questions/1_to_10

-- Needed for 9 and 10
import ReplaceHead

-- 1. Last (w/o using last)
myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- 2. Init (w/o using init)
myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast (x:[]) = error "List of length 1!"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

-- 3. elementAt (w/o using !!)
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index exceeds length of list"
elementAt (x:_) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

-- 4. length (w/o using length)
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5. reverse (w/o using reverse)
myReverse :: [a] -> [a]
myReverse xs = foldl flipPrepend [] xs
  where flipPrepend xs x = x:xs

myReverse' :: [a] -> [a]
myReverse' xs = (myLast xs):(myReverse (init xs))

-- 6. palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = take (floor ((fromIntegral (myLength xs)) / 2.0)) xs == take (floor ((fromIntegral (myLength xs)) / 2.0)) (reverse xs)

-- 7. flatten
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = foldl (++) (flatten x) [flatten x' | x' <- xs]

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- 8. Eliminate consecutive duplicates
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = foldl step [] xs
  where
    pushBack xs x = reverse (x:(reverse xs))
    step [] x = [x]
    step xs x = if myLast xs == x
                then xs
                else pushBack xs x

compress' :: Eq a => [a] -> [a]
compress' [] = []
compress' (x:xs) = if x == head xs
                   then compress' xs
                   else x:(compress' xs)

-- 9. Group consecutive duplicates
pack :: Eq a => [a] -> [[a]]
pack xs = reverse (step [] xs)
  where
    step :: Eq a => [[a]] -> [a] -> [[a]]
    step g [] = g
    step [] (x:xs) = step [[x]] xs
    step g (x:xs) = if (head (head g)) == x
                    then step (replaceHead (x:) g) xs
                    else step ([x]:g) xs

-- 10. Length encoding
encode :: (Eq a, Integral i) => [a] -> [(i, a)]
encode xs = reverse (encodeInit [] xs)
  where
    inc ::  (Integral i) => (i, a) ->  (i, a)
    inc (i, x) = (i + 1, x)

    encodeInit :: (Eq a, Integral i) => [(i, a)] -> [a] -> [(i, a)]
    encodeInit g [] = g
    encodeInit [] (x:xs) = encodeInit [(1, x)] xs
    encodeInit g (x:xs) = if (snd (head g)) == x
                           then encodeInit (replaceHead inc g) xs
                           else encodeInit ((1, x):g) xs



