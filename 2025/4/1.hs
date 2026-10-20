main = getContents >>= print . soln

soln input =
  length
    ( concatMap
        ( filter
            ( \(x, c@(i, j)) ->
                (x == '@')
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
                    < 4
            )
        )
        ( zipWith
            (\r i -> zipWith (\x j -> (x, (i, j))) r [0 ..])
            grid
            [0 ..]
        )
    )
  where
    grid = lines input

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