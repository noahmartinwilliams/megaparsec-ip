{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Megaparsec.IP.IPv6 where

import Control.Monad
import Text.Megaparsec as TM
import Text.Megaparsec.Char
import Data.IP.Internal
import Text.Megaparsec.IP.Types

hextet :: IPParser String
hextet = (try (TM.count 4 hexDigitChar) <|> try (TM.count 3 hexDigitChar) <|> try (TM.count 2 hexDigitChar) <|> try (TM.count 1 hexDigitChar))

basicIPv6 :: IPParser IPv6
basicIPv6 = do
    s <- sepBy1 hextet (single ':')
    let s' = Prelude.foldr (\x -> \y -> x ++ ":" ++ y) "" s
    let retAddr = read s' :: IPv6
    return retAddr

missingHextetIPv6 :: IPParser IPv6
missingHextetIPv6 = do
    s1 <- sepEndBy1 hextet (single ':')
    void $ single ':'
    s2 <- sepBy1 hextet (single ':')
    let s1' = Prelude.foldr (\x -> \y -> x ++ ":" ++ y ) "" s1
        s2' = Prelude.foldr (\x -> \y -> x ++ ":" ++ y ) "" s2
        s = s1' ++ ":" ++ s2' 
        retAddr = read s :: IPv6
    return retAddr

ipv6 :: IPParser IPv6
ipv6 = (try basicIPv6 <|> try missingHextetIPv6)
