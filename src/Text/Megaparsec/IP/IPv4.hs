module Text.Megaparsec.IP.IPv4(ipv4) where

import Control.Monad
import Data.IP.Internal
import Data.List.NonEmpty
import Data.Set
import Text.Megaparsec as TM
import Text.Megaparsec.Char
import Text.Megaparsec.IP.Types

subnet :: IPParser Int
subnet = do
    sn <- label "subnet" (some digitChar)
    let num = read sn :: Int
    if num > 255
    then do
        let (Just labl) = nonEmpty "subnet"
            (Just errorMsg) = nonEmpty "<=255."
        failure (Just (Label labl)) (Data.Set.fromList [Tokens errorMsg])
    else
        return num

ipv4 :: IPParser IPv4
ipv4 = do
    sn1 <- subnet
    void $ single '.'
    sn2 <- subnet
    void $ single '.'
    sn3 <- subnet
    void $ single '.'
    sn4 <- subnet
    let sn1s = show sn1
        sn2s = show sn2
        sn3s = show sn3
        sn4s = show sn4
        ips = sn1s ++ "." ++ sn2s ++ "." ++ sn3s ++ "." ++ sn4s
    return (read ips :: IPv4)
