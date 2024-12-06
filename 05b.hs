import Data.List
import Debug.Trace

main = do
    input <- readFile "05.in"
    let
        (rules,_:updates) = break null $ lines input
        way = parse '|' <$> rules
    print $ sum.(read.((!!)<*>(`div`2).length)<$>) <$> [[
            -- if any ((`isSubsequenceOf` ps).reverse) way
            if not $ null $ [x | x <- way, ((`isSubsequenceOf` ps).reverse) x]
            -- read on a forum that cycles are possible, just not within any update
            then sortBy (\a b -> if a==b then EQ else if [a,b] `elem` cl [p | p <- way, all (`elem` ps) p] then LT else GT) ps
            else ["0"]
            | ps <- parse ',' <$> updates]
        | part <- [0]]

-- \ps -> any ((`isSubsequenceOf` ps).reverse) way
-- partition (any.(isSubsequenceOf.reverse <$> way <*>))

parse sep =
    takeWhile (not.null)
    . unfoldr (Just . (drop 1 <$>) . span (/=sep))

cl rel =
    if length new > length rel
    then cl new
    else
        if (traceShowId$length new * 2) < let n = length (nub (concat new)) in traceShowId$n*(n-1)
        then cl $ (traceShowId$[h, head [x | x <- nub (concat new), x /= h, all (`notElem` new) [[h,x],[x,h]]]]) : new
        else rel
    where new@([h,_]:_) = union rel [[a,d] | [a,b]<-rel, [c,d]<-rel, b==c]

-- cl rel = if length new > length rel then cl new else rel
--     where new = union rel [[a,d] | [a,b]<-rel, [c,d]<-rel, b==c]

-- cl rel = if traceShowId$length new > length rel then cl new else rel
--     where
--     p = traceShowId$nub [[a,d] | [a,b]<-rel, [c,d]<-rel, b==c]
--     new = union rel p
