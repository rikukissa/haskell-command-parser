module Utils where

chunk' :: [a] -> [[a]]
chunk' [] = []
chunk' [x] = [[x]]
chunk' [x,y] = [[x, y]]
chunk' (x:y:xs) =
  [[x, y]] ++ (chunk' xs)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

boolToAnswer b = if b then "yes" else "no"
