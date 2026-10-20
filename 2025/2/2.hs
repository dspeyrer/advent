import Data.Array (Ix (range))

main =
  getContents
    >>= print
      . sum
      . concatMap
        ( filter
            ( \x ->
                let s = show x
                 in or
                      [ let a = chunks i s
                         in all (== head a) a
                        | i <- [1 .. (div (length s) 2)]
                      ]
            )
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

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n a = take n a : chunks n (drop n a)


