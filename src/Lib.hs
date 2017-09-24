module Lib
    ( someFunc
    ) where

import Lexer.Token
import Parser.Grammar
import Parser.AST
import Data.ByteString.Lazy.Char8 (pack)

debugPrint = fmap (show . PrintExp) . parseExp . pack

someFunc :: IO ()
someFunc = putStrLn "someFunc"
