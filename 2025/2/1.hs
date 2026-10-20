import Data.Array (Ix (range))

main =
  getContents
    >>= print
      . sum
      . concatMap
        ( filter (\x -> let s = show x in uncurry (==) $ splitAt (div (length s) 2) s)
            . range
            . pair
            . map (read :: String -> Integer)
            . splitBy (== '-')
        )
      . splitBy (== ',')

pair :: [a] -> (a, a)
pair [a, b] = (a, b)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c l = h : splitBy c (drop 1 t)
  where
    (h, t) = break c l