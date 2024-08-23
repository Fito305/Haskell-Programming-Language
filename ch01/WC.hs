-- lines beginning wit "--" are comments.
-- Word Count Command
main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
