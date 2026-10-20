import Control.Monad (when)

main = getContents >>= print . soln

soln input = countFull grid - countFull (consumeN grid)
  where
    grid = lines input

consumeN :: [String] -> [String]
consumeN grid
  | res == grid = res
  | otherwise = consumeN res
  where
    res = consume1 grid

consume1 :: [String] -> [String]
consume1 grid =
  map
    ( map
        ( \(x, c@(i, j)) ->
            if (x == '@')
              && length
                ( filter
                    (\(i, j) -> elem '@' $ (grid !? i) >>= (!? j))
                    [ p
                      | di <- [-1 .. 1],
                        dj <- [-1 .. 1],
                        let p = (i + di, j + dj),
                        p /= c
                    ]
                )
                >= 4
              then '@'
              else '.'
        )
    )
    ( zipWith
        (\r i -> zipWith (\x j -> (x, (i, j))) r [0 ..])
        grid
        [0 ..]
    )

countFull :: [String] -> Int
countFull = length . concatMap (filter ('@' ==))

xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n