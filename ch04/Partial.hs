isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
