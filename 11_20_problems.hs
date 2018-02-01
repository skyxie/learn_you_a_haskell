-- Solutions to 99 Questions: https://wiki.haskell.org/99_questions/11_to_20

-- Needed for 11
import ReplaceHead

-- 11. Encode-modified
data EncodeEntry a = Single a | Multiple Int a deriving (Show)

_encodeModifiedCmp :: Eq a => EncodeEntry a -> a -> Bool
_encodeModifiedCmp (Single x) y = x == y
_encodeModifiedCmp (Multiple _ x) y = x == y

_encodeEntryInc :: EncodeEntry a -> EncodeEntry a
_encodeEntryInc (Single x) = Multiple 2 x
_encodeEntryInc (Multiple i x) = Multiple (i+1) x

_encodeModifiedStep :: Eq a => [EncodeEntry a] -> [a] -> [EncodeEntry a]
_encodeModifiedStep g [] = g
_encodeModifiedStep [] (x:xs) = _encodeModifiedStep [Single x] xs
_encodeModifiedStep g (x:xs) = if _encodeModifiedCmp (head g) x
                               then _encodeModifiedStep (replaceHead _encodeEntryInc g) xs
                               else _encodeModifiedStep ((Single x):g) xs

encodeModified :: Eq a => [a] -> [EncodeEntry a]
encodeModified xs = reverse (_encodeModifiedStep [] xs)

-- 12. decodeModified
_decodeModifiedEntry :: EncodeEntry a -> [a]
_decodeModifiedEntry (Single x) = [x]
_decodeModifiedEntry (Multiple i x) = [x | _ <- [1..i]]

decodeModified :: [EncodeEntry a] -> [a]
decodeModified xs = foldl (++) [] [_decodeModifiedEntry x | x <- xs]

