newtype Parse a = Parse {
     runParse :: ParseState -> Either String (a, ParseState)
    }
