module Debug ( debug
             , printDebug
             , ifDebug
             ) where

debug :: Bool
debug = True

printDebug :: Show a => a -> IO ()
printDebug a = if debug then print a else return ()

ifDebug :: a -> a -> a
ifDebug a b = if debug then a else b
