module Main(main) where

import Text.Megaparsec.IP.IPv6
import Text.Megaparsec
import Data.Void
import Data.Text as T
import Data.IP.Internal
import Data.Either

test01 :: IO ()
test01 = do
    let basic = "2001:0DB8:0000:0000:A222:B333:0000:ABCD"
        res = parse basicIPv6 "" (T.pack basic)
        res2 = read basic :: IPv6
    if isRight res
    then do
        let (Right res') = res
        if res' /= res2
        then
            putStrLn " Test 01 failed."
        else
            putStrLn " Test 01 Succeeded."
    else do
        let (Left res') = res
        putStrLn (errorBundlePretty res')

test02 :: IO ()
test02 = do
    let basic = "2001:0DB8::A222:B333:0000:ABCD"
        res = parse missingHextetIPv6 "" (T.pack basic)
        res2 = read basic :: IPv6
    if isRight res
    then do
        let (Right res') = res
        if res' /= res2
        then
            putStrLn " Test 02 failed."
        else
            putStrLn " Test 02 Succeeded."
    else do
        let (Left res') = res
        putStrLn (errorBundlePretty res')

main :: IO ()
main = do
    test01
    test02
