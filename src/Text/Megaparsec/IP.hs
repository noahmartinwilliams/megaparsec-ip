module Text.Megaparsec.IP(module V6, 
    module V4,
    module Text.Megaparsec.IP.Types,
    ip) where

import Data.IP
import Data.IP.Internal
import Text.Megaparsec
import Text.Megaparsec.IP.IPv4 as V4
import Text.Megaparsec.IP.IPv6 as V6
import Text.Megaparsec.IP.Types

ip1 :: IPParser IP
ip1 = do
    i <- V4.ipv4
    return (IPv4 i)

ip2 :: IPParser IP 
ip2 = do
    i <- V6.ipv6
    return (IPv6 i)

ip :: IPParser IP
ip = ip1 <|> ip2
