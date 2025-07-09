{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.IP.IPv6 where

import Control.Monad
import Text.Megaparsec as TM
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text as T
import Data.Void
import Data.IP.Internal

hexadecimal :: (MonadParsec Void s m, Token s ~ Char) => m (Token s)
hexadecimal = (digitChar <|> char 'a' <|> char 'b' <|> char 'c' <|> char 'd' <|> char 'e' <|> char 'f' <|> char 'A' <|> char 'B' <|> char 'C' <|> char 'D' <|> char 'E' <|> char 'F' )


hextet :: (MonadParsec Void s m, Token s ~ Char) => m [(Token s)]
hextet = (TM.count 4 hexadecimal <|> TM.count 3 hexadecimal <|> TM.count 2 hexadecimal <|> TM.count 1 hexadecimal)

hextetColon :: (Token s ~ Char, MonadParsec Void s m) => m [(Token s)]
hextetColon = do
    ht <- hextet
    void $ single ':'
    return (ht ++ [':'])

basicIPv6 :: (Token s ~ Char, MonadParsec Void s m) => m IPv6
basicIPv6 = do
    s <- sepBy1 hextet (single ':')
    let s' = Prelude.foldr (\x -> \y -> x ++ ":" ++ y) "" s
    let retAddr = read s' :: IPv6
    return retAddr

missingHextetIPv6 :: (Token s ~ Char, MonadParsec Void s m) => m IPv6
missingHextetIPv6 = do
    s1 <- sepEndBy1 hextet (single ':')
    void $ single ':'
    s2 <- sepBy1 hextet (single ':')
    let s1' = Prelude.foldr (\x -> \y -> x ++ ":" ++ y ) [] s1
        s2' = Prelude.foldr (\x -> \y -> x ++ ":" ++ y ) [] s2
        s = s1' ++ [':'] ++ s2' 
        retAddr = read s :: IPv6
    return retAddr
