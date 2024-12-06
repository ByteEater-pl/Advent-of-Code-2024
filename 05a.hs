import Data.List
import Debug.Trace

main = do
    input <- readFile "05.in"
    let
        (rules,_:updates) = break null $ lines input
        way = parse '|' <$> rules
    print $ sum.(read.((!!)<*>(`div`2).length)<$>) <$> [[
            if any ((`isSubsequenceOf` ps).reverse) way
            then ["0"]
            else part ps
            | ps <- parse ',' <$> updates]
        | part <- [
            id]]
            -- sortBy (\a b -> if [a,b] `elem` cl way then LT else GT)]]

-- \ps -> any ((`isSubsequenceOf` ps).reverse) way
-- partition (any.(isSubsequenceOf.reverse <$> way <*>))

parse sep =
    takeWhile (not.null)
    . unfoldr (Just . (drop 1 <$>) . span (/=sep))

cl rel = if length new > length rel then cl new else rel
    where new = union rel [[a,d] | [a,b]<-rel, [c,d]<-rel, b==c]

-- cl rel = if traceShowId$length new > length rel then cl new else rel
--     where
--     p = traceShowId$nub [[a,d] | [a,b]<-rel, [c,d]<-rel, b==c]
--     new = union rel p
