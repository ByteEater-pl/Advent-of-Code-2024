import Data.List

main = do
    input <- readFile "02.in"
    print [
        length
        $ filter (
            \report -> or [
                all (`elem` map (*s) [1..3]) (zipWith (-) l t)
                | s <- [1,-1],
                  l@(_:t) <- variants report])
        $ map (map read . words)
        $ lines input
        | variants <-
            [(:[]), \l -> zipWith (++) (inits l) (drop 1 <$> tails l)]]
