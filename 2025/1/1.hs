main :: IO ()
main =
  getContents
    >>= print
      . length
      . filter (== 0)
      . ( scanl
            (\x y -> mod (x + y) 100)
            50
            . map (\(x : xs) -> (read xs :: Integer) * if x == 'R' then 1 else -1)
        )
      . lines
