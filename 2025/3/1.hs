main =
    getContents >>= print . sum . map (\x ->
        let h = maximum $ init x
        in read (h : [maximum $ tail $ dropWhile (h /=) x]) :: Integer
    ) . lines
      
      

