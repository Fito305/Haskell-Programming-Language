{-# LANGUAGE DeriveDataTypeable #-}


import Data.Dynamic
import Control.Exception

data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
                deriving (Eq, Show, Read, Typeable)

{- | Execute the given IO action.
 - If it raises a 'SqlError', then execute the supplied
 - handler and return its return value. Otherwise, proceed
 - as normal. -}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn

{- | Like 'catchSql', with the order of arguments reversed. -}
handleSql :: {SqlError -> IO a) -> IO a -> IO a
handleSql = flip catchSql
