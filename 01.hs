import Data.List

main = do
    input <- readFile "01.in"
    let ls = sort . map read <$> transpose (words <$> lines input)
    print
        (sum [abs (a-b) | [a,b] <- transpose ls],
        score ls)

score ls@(~[(h:t),l]) =
    if any null ls
    then 0
    else h * length i + score [t,r]
    where (i,r) = span (==h) $ dropWhile (<h) l
