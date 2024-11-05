module Logger
    (
       Logger
     , Log
     , runLogger
     , record
    ) where


globToRegex :: String -> Logger String
type Log = [String]
runLogger :: Logger a -> (a, Log)
record :: String -> Logger ()
