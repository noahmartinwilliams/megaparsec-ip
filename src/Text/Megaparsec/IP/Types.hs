module Text.Megaparsec.IP.Types where

import Text.Megaparsec 
import Data.Void

type IPParser = Parsec Void String 
