{-# LANGUAGE TypeApplications #-}

import Data.Array.Unboxed

main = do
    input <- lines <$> readFile "04.in"
    let
        n = length input
        r = ((1,1),(n,n))
        d = range ((-1,-1),(1,1))
        crossword = listArray @UArray r (concat input)
    print $ length . filter id <$> [
        [and [
            inRange r c && crossword ! c == "XMAS" !! m
            | m <- [0..3],
              let c = (i+m*k,j+m*l)]
        | (i,j) <- range r,
          (k,l) <- d],
        [[crossword ! (i+k,j+l) | (k,l) <- map (d!!) [0,2..8]] `elem`
            ["MSAMS", "SSAMM", "SMASM", "MMASS"]
        | (i,j) <- range ((2,2),(n-1,n-1))]]
