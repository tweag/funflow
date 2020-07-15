{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib where

import Inliterate.Import
import System.IO.Unsafe

-- Required to run and display IO
instance (Show a) => AskInliterate (IO a) where
    askInliterate q _ io = do
        putStrLn "<div class=\"row\">"
        putStrLn "<div class=\"col-md-1\">"
        putStrLn "In"
        putStrLn "</div>"
        putStrLn "<div class=\"col-md-11\">"
        putStr "<pre class=\"haskell\"><code>"
        putStrLn $ q
        putStrLn "</code></pre>"
        putStrLn "</div>"
        putStrLn "</div>"
        putStrLn "<div class=\"row\">"
        putStrLn "<div class=\"col-md-1\">"
        putStrLn "Out"
        putStrLn "</div>"
        putStrLn "<div class=\"col-md-11\">"
        putStr "<pre class=\"text\"><code>"
        putStrLn .show . unsafePerformIO $ io
        putStrLn "</code></pre>"
        putStrLn "</div>"
        putStrLn "</div>"