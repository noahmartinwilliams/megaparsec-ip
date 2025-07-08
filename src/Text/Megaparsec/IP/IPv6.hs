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
hextet = TM.count 4 hexadecimal 

hextetColon :: (Token s ~ Char, MonadParsec Void s m) => m [(Token s)]
hextetColon = do
    ht <- hextet
    void $ single ':'
    return (ht ++ [':'])

basicIPv6 :: (Token s ~ Char, MonadParsec Void s m) => m IPv6
basicIPv6 = do
    s1 <- TM.count 7 hextetColon
    s2 <- hextet
    let s1' = Prelude.foldr (++) [] s1
        addr = s1' ++ s2
        retAddr = read addr :: IPv6
    return retAddr
