module ReplaceHead where 
  replaceHead :: (a -> a) -> [a] -> [a]
  replaceHead f (x:xs) = (f x):xs
