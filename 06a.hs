{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Array.IO
import Data.List

main = do
    input <- lines <$> readFile "06.in"
    let
        n = length input
        r = ((1,1),(n,n))
    area <- newListArray @IOUArray r $ concat input
    Just (pos,_) <- find ((=='^').snd) <$> getAssocs area
    let loop p@(x,y) dx dy = do
        writeArray area p 'X'
        let p' = (x-dx,y-dy)
        when @IO (inRange r p') $ do
            next <- readArray area p'
            if next == '#'
            then loop p dy (-dx)
            else loop p' dx dy
    loop pos 1 0
    print . length . filter (=='X') =<< getElems area
