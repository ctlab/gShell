module Debug ( debug
             , printDebug
             ) where

debug :: Bool
debug = True

printDebug :: String -> IO ()
printDebug a = if debug then print a else return ()
