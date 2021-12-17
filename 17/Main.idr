import Data.List

Input : Type
Input = ((Int, Int), (Int, Int))

part1 : Input -> IO String
part1 (_, (ymin, _)) = pure $ show $ (\v => (v * (v + 1)) `div` 2) $ abs $ ymin + 1

inTarget : (Int, Int) -> Input -> (Int, Int) -> Bool
inTarget (x, y) i@((xmin, xmax), (ymin, ymax)) (vx, vy) =
  (xmin <= x && x <= xmax && ymin <= y && y <= ymax) ||
  (x <= xmax && y >= ymin && inTarget (x + vx, y + vy) i (max 0 (vx - 1), vy - 1))

part2 : Input -> IO String
part2 i@((xmin, xmax), (ymin, ymax)) = do let xs = rangeFromTo 0 xmax
                                          let ys = rangeFromTo ymin (abs $ ymin + 1)
                                          pure $ show $ length $ filter (inTarget (0,0) i) $ join $ (\x => (x,) <$> ys) <$> xs

main : IO ()
main = do let a = ((14, 50), (-267, -225))
          part1 a >>= putStrLn
          part2 a >>= putStrLn

