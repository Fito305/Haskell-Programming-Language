module SimpleJSON
(
 JValue(..)
 , getString
 , getInt
 , getDouble
 , getBool
 , getObject
 , getArray
 , isNull
 ) where

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

-- For each JSON type, we supply a distinct value constructor.
-- Some of these constructors have parameters: if we want to construct
-- a JSON string, we must provide a String value as an argument to the JString constructor.

-- To do the the reverse, we use pattern matching. Here's a function that we can add that will extract
-- a string from a JSON value for us. If the JSON value actually contains a string, our function will
-- wrap the string with the Just constructor, otherwise it will return Nothing.

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

-- A few more accessor functions and we've got a small body of code to work with.
-- The truncate function returns a floating-point or rational number into an integer by 
-- dropping the digits after the decimal point.
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull
