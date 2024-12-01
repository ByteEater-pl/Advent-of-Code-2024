import Data.List

main = do
    ls <- lines <$> readFile "01.in"
    let ns = (sort . map read) <$> transpose (words <$> ls)
    print
        (sum [abs (a-b) | [a,b] <- transpose ns],
        score ns)

score ns@(~[(h:t),l]) =
    if any null ns
    then 0
    else h * length i + score [t,r]
    where (i,r) = span (==h) $ dropWhile (<h) l
