module Text.Megaparsec.IP.Types where

import Text.Megaparsec 
import Data.Text
import Data.Void

type Parser = Parsec Void Text 
