main :: IO ()
main =
  getContents
    >>= print
      . length
      . filter (== 0)
      . foldl
        folder
        [50]
      . vals

folder :: [Int] -> Int -> [Int]
folder hist inc
  | inc == 0 = hist
  | otherwise = folder (mod (head hist + dir) 100 : hist) (inc - dir)
  where
    dir = signum inc

vals :: String -> [Int]
vals s =
  map
    (\(x : xs) -> (read xs :: Int) * if x == 'R' then 1 else -1)
    (lines s)


 