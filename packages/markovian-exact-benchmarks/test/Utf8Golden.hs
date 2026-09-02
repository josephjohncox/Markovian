module Utf8Golden (readUtf8File) where

import Control.Exception (evaluate)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, utf8, withFile)

readUtf8File :: FilePath -> IO String
readUtf8File path = withFile path ReadMode $ \handle -> do
    hSetEncoding handle utf8
    contents <- hGetContents handle
    _ <- evaluate (length contents)
    pure contents
