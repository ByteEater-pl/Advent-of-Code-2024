import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

main = do
    input <- readFile "03.in"
    print [
        sum [a*b | Just (a,b) <-
            fst $
            head $
            readP_to_S
                (many (
                    ((Just.).(,)
                        <$> (string "mul(" *> number)
                        <*> (char ',' *> number <* char ')'))
                    <++ skip
                    <++ (Nothing <$ get)
                    ) <* eof)
                input]
        | skip <- [
            pfail,
            Nothing
            <$ string "don't()"
            <* manyTill get (void (string "do()") <++ eof)]]

number = read <$> (mfilter ((`elem` [1..3]).length) $ many $ satisfy isDigit)
