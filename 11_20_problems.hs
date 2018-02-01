-- Solutions to 99 Questions: https://wiki.haskell.org/99_questions/11_to_20

-- Needed for 11 - 13
import ReplaceHead

-- Needed for 11 - 13
data EncodeEntry a = Single a | Multiple Int a deriving (Show)

-- Redefined for 11
encode :: Eq a => [a] -> [(Int, a)]
encode xs = reverse (encodeInit [] xs)
  where
    inc :: (Int, a) ->  (Int, a)
    inc (i, x) = (i + 1, x)

    encodeInit :: Eq a => [(Int, a)] -> [a] -> [(Int, a)]
    encodeInit g [] = g
    encodeInit [] (x:xs) = encodeInit [(1, x)] xs
    encodeInit g (x:xs) = if (snd (head g)) == x
                           then encodeInit (replaceHead inc g) xs
                           else encodeInit ((1, x):g) xs

-- 11. encodeModified
encodeModified :: Eq a => [a] -> [EncodeEntry a]
encodeModified xs = [entry x | x <- (encode xs)]
  where
    entry :: (Int, a) -> EncodeEntry a
    entry (1, x) = Single x
    entry (i, x) = Multiple i x

-- 12. decodeModified
decodeModified :: [EncodeEntry a] -> [a]
decodeModified xs = foldl (++) [] [decodeEntry x | x <- xs]
  where
    decodeEntry :: EncodeEntry a -> [a]
    decodeEntry (Single x) = [x]
    decodeEntry (Multiple i x) = [x | _ <- [1..i]]

-- 13. encode-direct
encodeDirect :: Eq a => [a] -> [EncodeEntry a]
encodeDirect xs = reverse (step [] xs)
  where
    cmp :: Eq a => EncodeEntry a -> a -> Bool
    cmp (Single x) y = x == y
    cmp (Multiple _ x) y = x == y

    inc :: EncodeEntry a -> EncodeEntry a
    inc (Single x) = Multiple 2 x
    inc (Multiple i x) = Multiple (i+1) x

    step :: Eq a => [EncodeEntry a] -> [a] -> [EncodeEntry a]
    step g [] = g
    step [] (x:xs) = step [Single x] xs
    step g (x:xs) = if cmp (head g) x
                    then step (replaceHead inc g) xs
                    else step ((Single x):g) xs

