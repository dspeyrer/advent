import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Sequence (fromList, (!?))

counter :: (MonadState b m, Num b) => a -> m (a, b)
counter x = do
  n <- get
  put (n + 1)
  return (x, n)

enumerate a = evalState (mapM counter a) 0

coordinate a = concatMap (\(l, i) -> map (\(x, j) -> (x, (i, j))) l) $ enumerate (map enumerate a)

main = getContents >>= print . soln

soln input =
  length
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
        grid
    )
  where
    grid = fromList $ coordinate $ lines input