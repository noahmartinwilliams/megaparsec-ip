{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Megaparsec.IP.IPv6 where

import Control.Monad
import Text.Megaparsec as TM
import Text.Megaparsec.Char
import Data.Text as T
import Data.IP.Internal
import Text.Megaparsec.IP.Types

hextet :: Parser Text
hextet = pack <$> (TM.count 4 hexDigitChar <|> TM.count 3 hexDigitChar <|> TM.count 2 hexDigitChar <|> TM.count 1 hexDigitChar)

basicIPv6 :: Parser IPv6
basicIPv6 = do
    s <- sepBy1 hextet (single ':')
    let s' = Prelude.foldr (\x -> \y -> x `append` (T.pack ":") `append` y) (T.pack "") s
    let retAddr = read (T.unpack s') :: IPv6
    return retAddr

missingHextetIPv6 :: Parser IPv6
missingHextetIPv6 = do
    s1 <- sepEndBy1 hextet (single ':')
    void $ single ':'
    s2 <- sepBy1 hextet (single ':')
    let s1' = Prelude.foldr (\x -> \y -> x `append` ":" `append` y ) (T.pack "") s1
        s2' = Prelude.foldr (\x -> \y -> x `append` ":" `append` y ) (T.pack "") s2
        s = s1' `append` (T.pack ":") `append` s2' 
        retAddr = read (T.unpack s) :: IPv6
    return retAddr
