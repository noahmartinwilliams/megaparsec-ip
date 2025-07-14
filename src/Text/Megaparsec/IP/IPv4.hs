module Text.Megaparsec.IP.IPv4 where

import Text.Megaparsec as TM
import Text.Megaparsec.Char
import Text.Megaparsec.IP.Types
import Data.Set
import Data.Text as T
import Data.List.NonEmpty

subnet :: Parser Int
subnet = do
    sn <- label "subnet" (some digitChar) -- (TM.count 1 digitChar <|> TM.count 2 digitChar <|> TM.count 3 digitChar)
    let num = read sn :: Int
    if num > 255
    then do
        let (Just labl) = nonEmpty "subnet"
            (Just errorMsg) = nonEmpty "<=255."
        failure (Just (Label labl)) (Data.Set.fromList [Tokens errorMsg])
    else
        return num

