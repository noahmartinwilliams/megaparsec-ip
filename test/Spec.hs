module Main(main) where

import Text.Megaparsec.IP.IPv6
import Text.Megaparsec
import Data.Void
import Data.Text as T
import Data.IP.Internal

test01 :: IO ()
test01 = do
    let basic = "2001:0DB8:0000:0000:A222:B333:0000:ABCD"
        (Right res) = parse basicIPv6 "" (T.pack basic)
        res2 = read basic :: IPv6
    if res /= res2
    then
        putStrLn " Test 01 failed."
    else
        putStrLn " Test 01 Succeeded."

main :: IO ()
main = do
    test01
