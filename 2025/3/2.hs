main =
  getContents
    >>= print
      . sum
      . map
        (\x -> read (soln 12 x) :: Integer)
      . lines

soln :: Int -> String -> String
soln 0 _ = []
soln n a = h : soln (n - 1) (tail (dropWhile (h /=) a))
  where
    h = maximum (take (length a - n + 1) a)
    
    