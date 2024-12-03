import Control.Monad
import Data.Char
import Data.Functor
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
                    <++ (get $> Nothing)
                    ) <* eof)
                input]
        | skip <- [
            pfail,
            string "don't()"
            *> manyTill get (void (string "do()") <++ eof)
            $> Nothing]]

number = read <$> (mfilter ((`elem` [1..3]).length) $ many $ satisfy isDigit)
